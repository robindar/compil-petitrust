open Ast
open Typed_ast

exception Borrow_checking_error of Ast.location * string

let is_copy_type = function
  | Unit | Int32 | Boolean | Ref (false, _) -> true
  | _ -> false
let is_move_type = function
  | Vect _ | Ref (true, _) | Struct _ -> true
  | _ -> false

module SSet = Set.Make(String)

(* borrow_status : moved, borrow, mutably borrowed *)
type borrow_status = SSet.t * SSet.t * SSet.t
let empty_borrow_status = (SSet.empty, SSet.empty, SSet.empty)

let move (m_set, b_set, mb_set) v = (SSet.add v m_set, b_set, mb_set)
let borrow (m_set, b_set, mb_set) v = (m_set, SSet.add v b_set, mb_set)
let mutborrow (m_set, b_set, mb_set) v = (m_set, b_set, SSet.add v mb_set)

let unmove (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)
let unborrow (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)
let unmutborrow (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)

let is_moved (set, _, _) v = SSet.mem v set
let is_borrowed (_, set, _) v = SSet.mem v set
let is_mutborrowed (_, _, set) v = SSet.mem v set

let clear set v = unmove (unborrow (unmutborrow set v) v) v

let merge_status (a1,b1,c1) (a2,b2,c2) =
  (SSet.union a1 a2, SSet.union b1 b2, SSet.union c1 c2)

let rec extract_ident = function
  | TIdent (i, _, _) -> i
  | TDot (e, i, _, _) -> (extract_ident e) ^ "." ^ i
  | _ -> raise Not_found

let is_moved_expr set e =
  try
    is_moved set (extract_ident e)
  with Not_found -> false
let is_borrowed_expr set e =
  try
    is_borrowed set (extract_ident e)
  with Not_found -> false
let is_mutborrowed_expr set e =
  try
    is_mutborrowed set (extract_ident e)
  with Not_found -> false

let rec _check_usable_expr (m,b,mb) set =
  let _check_var e =
    if m && is_moved_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot use variable because is was moved"))
    else if b && is_borrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot use variable because it is borrowed"))
    else if mb && is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
        "Cannot use variable because it is mutably borrowed"))
  in function
  | TLen (ie,_,_)
  | TUnop (Minus,ie,_,_) -> check_usable_expr set ie
  | TBinop (Equal,_,_,_,_) -> ()
  | TBrackets (e1,e2,_,_)
  | TBinop (_,e1,e2,_,_) ->
      check_usable_expr set e1;
      check_usable_expr set e2
  | TFunCall (_,iel,_,_)
  | TVec (iel,_,_) -> List.iter (check_usable_expr set) iel
  | TInt _ | TBool _ | TUnop _ | TPrint _ | TBloc _ -> ()
  | TDot (ie,_,_,_) as e -> check_usable_expr set ie; _check_var e
  | e -> _check_var e
and check_usable_expr set = _check_usable_expr (true, true, true) set

let check_moveable_expr set e =
  let _check_var e =
    if is_moved_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot use variable because is was moved"))
    else if is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot move out of variable because it is borrowed"))
    else if is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot move out of variable because it is mutably borrowed"))
  in match e with
    | TIdent _ -> _check_var e
    | TDot (ie,_,_,_) -> check_usable_expr set ie; _check_var e
    | _ -> check_usable_expr set e
let check_borrowable_expr set e =
  let _check_var e =
    if is_moved_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot use variable because is was moved"))
    else if is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot borrow variable as immutable because it was
            already borrowed as mutable"))
  in match e with
  | TIdent _ -> _check_var e
  | TDot (ie,_,_,_) -> check_usable_expr set ie; _check_var e
  | TBrackets (eo,ei,_,_) ->
      check_usable_expr set eo;
      check_usable_expr set ei;
      _check_var eo
  | _ -> check_usable_expr set e
let check_mutborrowable_expr set e =
  let _check_var e =
    if is_moved_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot use variable because it was moved"))
    else if is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot borrow variable as mutable more than once at a time"))
    else if is_mutborrowed_expr set e then
      raise (Borrow_checking_error (Typer.location_of_expr e,
            "Cannot borrow variable as mutable because it was
            already borrowed as immutable"))
  in match e with
  | TIdent _ -> _check_var e
  | TDot (ie,_,_,_) -> check_usable_expr set ie; _check_var e
  | TBrackets (eo,ei,_,_) ->
      check_usable_expr set eo;
      check_usable_expr set ei;
      _check_var eo
  | _ -> check_usable_expr set e

let rec move_expr set e =
  check_moveable_expr set e;
  try match e with
    | TVec (tel,_,_) | TFunCall (_,tel,_,_) ->
        List.fold_left
          (fun _set x -> match Typer.type_of_expr x with |Vect _ |Struct _ -> move_expr _set x |_ -> _set)
        set tel
    | _ -> if is_move_type (Typer.type_of_expr e) then
             move set (extract_ident e)
           else set
  with Not_found -> set
let unmove_expr set e =
  try
    unmove set (extract_ident e)
  with Not_found -> set
let rec borrow_expr set e =
  check_borrowable_expr set e;
  try match e with
  | TBrackets (te,_,_,_) -> borrow_expr set te
  | _ -> borrow set (extract_ident e)
  with Not_found -> set
let rec mutborrow_expr set e =
  check_mutborrowable_expr set e;
  try match e with
  | TBrackets (te,_,_,_) -> borrow_expr set te
  | _ -> mutborrow set (extract_ident e)
  with Not_found -> set


let borrow_check_file file =
  let rec borrow_check_expr (set : borrow_status) = function
    | TBinop (Equal, e1, e2, _, _) ->
        _check_usable_expr (false, true, true) set e1;
        check_usable_expr set e2;
        let _set = match e2 with
          | TUnop (Amp,e,_,_) -> borrow_expr set e
          | TUnop (AmpMut,e,_,_) -> mutborrow_expr set e
          | e -> move_expr set e in
        unmove_expr _set e1
    | TBloc (bloc, _, _) -> borrow_check_bloc set bloc
    | e -> check_usable_expr set e; set
  and borrow_check_instr (set : borrow_status) = function
    | TEmpty _ -> set
    | TExpr (e, _, _) -> move_expr (borrow_check_expr set e) e
    | TLet (_, (i,i_loc), te, _, _) ->
        let _set = match te with
          | TUnop (Amp,e,_,_) -> borrow_expr set e
          | TUnop (AmpMut,e,_,_) -> mutborrow_expr set e
          | e -> move_expr set e in
        unmove _set i
    | TLetStruct (_, (i,i_loc), _, var_list, _, _) ->
        let _set =
          List.fold_left
            (fun _set (_,_,te) ->
              match te with
              | TUnop (Amp,e,_,_) -> borrow_expr set e
              | TUnop (AmpMut,e,_,_) -> mutborrow_expr set e
              | e -> move_expr set e)
            set var_list in
        unmove _set i
    | TWhile (c, b, _, _) ->
        let _set = borrow_check_bloc (borrow_check_expr set c) b in
        let _ = borrow_check_bloc (borrow_check_expr _set c) b in
        merge_status _set set
    | TReturn (eo, _, _) ->
        begin
          match eo with
          | None -> set
          | Some e -> borrow_check_expr set e
        end
    | TIf (c, t, e, _, _) ->
        let c_set = borrow_check_expr set c in
        let t_set = borrow_check_bloc c_set t
        and e_set = borrow_check_bloc c_set e in
        merge_status t_set e_set
  and borrow_check_bloc (set : borrow_status) (instr, eopt, _, _) =
    let _set = List.fold_left
      (fun _set i -> borrow_check_instr _set i)
      set instr in
    match eopt with
    | None -> set
    | Some e -> let _ = borrow_check_expr _set e in set
  and borrow_check_decl = function
    | TDeclStruct ((s,s_loc), var_list, _, _) -> ()
    | TDeclFun ((f,f_loc), arg_list, _, b, _, _) ->
        let n_set =
          List.fold_left
            (fun _set (_, i, _, _) -> clear _set i)
           empty_borrow_status arg_list in
        let _ = borrow_check_bloc n_set b in () in
  List.iter borrow_check_decl file
