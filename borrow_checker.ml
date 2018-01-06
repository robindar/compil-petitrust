open Ast
open Typed_ast

exception Borrow_checking_error of Ast.location * string

let is_copy_type = function
  | Unit | Int32 | Boolean | Ref (false, _) -> true
  | _ -> false
let is_move_type = function
  | Vect _ | Ref (true, _) -> true
  | _ -> false

module SSet = Set.Make(String)

(* borrow_status : moved, borrow, mutably borrowed *)
type borrow_status = SSet.t * SSet.t * SSet.t
let empty_borrow_status = (SSet.empty, SSet.empty, SSet.empty)

let move (m_set, b_set, mb_set) v = (SSet.add v m_set, b_set, mb_set)
let borrow (m_set, b_set, mb_set) v = (m_set, SSet.add v b_set, mb_set)
let mborrow (m_set, b_set, mb_set) v = (m_set, b_set, SSet.add v mb_set)

let unmove (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)
let unborrow (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)
let unmborrow (m_set, b_set, mb_set) v = (SSet.remove v m_set, b_set, mb_set)

let is_moved (set, _, _) v = SSet.mem v set
let is_borrowed (_, set, _) v = SSet.mem v set
let is_mborrowed (_, _, set) v = SSet.mem v set

let clear set v = unmove (unborrow (unmborrow set v) v) v

let merge_status (a1,b1,c1) (a2,b2,c2) =
  (SSet.union a1 a2, SSet.union b1 b2, SSet.union c1 c2)

let rec move_expr set = function
  | TIdent (i, _, _) -> move set i
  | TFunCall (_, tel, _, _) -> List.fold_left move_expr set tel
  | _ -> set
let unmove_expr set = function
  | TIdent (i, _, _) -> unmove set i
  | _ -> set

let rec extract_ident = function
  | TIdent (i, _, _) -> i
  | TDot (e, i, _, _) -> (extract_ident e) ^ "." ^ i
  | TBrackets (eo, ei, _, _) -> extract_ident eo
  | _ -> raise Not_found

let check_r_value set i l =
  if is_mborrowed set i then
    raise (Borrow_checking_error (l,
          "Cannot use " ^ i ^ " because it was mutably borrowed"))
  else if is_borrowed set i then
    raise (Borrow_checking_error (l,
          "Cannot use " ^ i ^ " because it was borrowed"))
  else if is_moved set i then
    raise (Borrow_checking_error (l,
          "Cannot use " ^ i ^ " because it was moved"))

let lprocess set = function
  | TBrackets (eo, ei, l, _) ->
      let i = extract_ident eo in
      if is_borrowed set i then
        raise (Borrow_checking_error (l,
              "Cannot borrow " ^ i ^ " as mutable because it is already
               borrowed as immutable"))
      else if is_mborrowed set i then
        raise (Borrow_checking_error (l,
              "Cannot borrow " ^ i ^ " as mutable more than once at a time"))
  | TIdent (i, l, _) ->
      if is_mborrowed set i || is_borrowed set i then
        raise (Borrow_checking_error (l,
              "Cannot assign to " ^ i ^ " because it is borrowed"))
  | _ -> ()

let rec rprocess _set = function
  | TIdent (i, l, _) ->
      check_r_value _set i l; move _set i
  | TFunCall (_, tel, l, _) ->
      List.fold_left rprocess _set tel
  | TBrackets (eo, ei, l, _) ->
      check_r_value _set (extract_ident eo) l; _set
  | TDot (e, i, l, _) as te ->
      check_r_value _set (extract_ident te) l; _set
  | _ -> _set

let process_equality set le re =
  lprocess set le;
  rprocess set re

let borrow_check_file file =
  let rec borrow_check_expr (set : borrow_status) = function
    | TBinop (Equal, e1, e2, _, _) -> process_equality set e1 e2
    | TBloc (bloc, _, _) -> borrow_check_bloc set bloc
    | e -> rprocess set e
  and borrow_check_instr (set : borrow_status) = function
    | TEmpty _ -> set
    | TExpr (e, _, _) -> borrow_check_expr set e
    | TLet (_, (i,i_loc), te, _, _) ->
        begin
          let _set = rprocess set te in
          let __set = match te with
            | TUnop(Amp,e,_,_) -> borrow _set (extract_ident e)
            | TUnop(AmpMut,e,_,_) -> mborrow _set (extract_ident e)
            | _ -> _set in
          unmove __set i
        end
    | TLetStruct (_, (i,i_loc), _, var_list, _, _) -> assert false
    | TWhile (c, b, _, _) ->
        let _set = borrow_check_bloc (borrow_check_expr set c) b in
        let _ = borrow_check_bloc _set b in
        merge_status _set set
    | TReturn (eo, _, _) ->
        begin
          match eo with
          | None -> set
          | Some e -> borrow_check_expr set e
        end
    | TIf (c, t, e, _, _) -> assert false
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
