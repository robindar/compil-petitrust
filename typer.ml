open Ast
open Typed_ast

exception Typing_error of location * string

module Env = Map.Make(String)

let empty_env = (Env.empty, Env.empty, Env.empty)

let var_type (env, _, _) i = fst (Env.find i env)
let is_mut (env, _, _) i = snd (Env.find i env)
let is_fun (_, env, _) f = Env.mem f env
let fun_type (_, env, _) f = Env.find f env
let is_struct (_, _, env) s = Env.mem s env
let has_variable (_, _, env) s i = Env.mem i (Env.find s env)
let struct_type (_, _, env) s i = Env.find i (Env.find s env)

let decl_var (env, f_env, s_env) v t mut = (Env.add v (t, mut) env, f_env, s_env)
let decl_fun (v_env, env, s_env) f t = (v_env, Env.add f t env, s_env)
let decl_struct (v_env, f_env, env) s l =
  let s_env = List.fold_left
              (fun e (i,_,t) -> Env.add i t e)
              Env.empty l in
  (v_env, f_env, Env.add s s_env env)

let check_duplicate_idents l =
  let add env = function
    | (x, l, _) when Env.mem x env ->
      raise (Typing_error (l, "Duplicate variable name in definition : " ^ x))
    | (x, l, _) -> Env.add x () env in
  let _ = List.fold_left add Env.empty l in ()

let unop_type = function
  | Minus  -> [Int32], Int32
  | Bang   -> [Boolean], Boolean
  | Star   -> assert false
  | Amp    -> assert false
  | AmpMut -> assert false

let binop_type = function
  | Add | Sub | Mul | Div | Mod -> [Int32; Int32], Int32
  | Equal -> assert false
  | Eq | Neq | Leq | Geq | Lt | Gt -> [Int32; Int32], Boolean
  | And | Or -> [Boolean; Boolean], Boolean

let type_of_expr = function
    TInt (_, _, t) | TBool (_, _, t) | TIdent (_, _, t)
  | TUnop (_, _, _, t) | TBinop (_, _, _, _, t)
  | TDot (_, _, _, t) | TLen (_, _, t) | TBrackets (_, _, _, t)
  | TFunCall (_, _, _, t) | TVec (_, _, t) | TPrint (_, _, t)
  | TBloc (_, _, t) -> t
let type_of_bloc = function
  | _, _, _, t -> t
let location_of_expr = function
    TInt (_, l, _) | TBool (_, l, _) | TIdent (_, l, _)
  | TUnop (_, _, l, _) | TBinop (_, _, _, l, _)
  | TDot (_, _, l, _) | TLen (_, l, _) | TBrackets (_, _, l, _)
  | TFunCall (_, _, l, _) | TVec (_, l, _) | TPrint (_, l, _)
  | TBloc (_, l, _) -> l

let type_keywords = [ "i32", Int32; "()", Unit; "bool", Boolean]
let rec expr_type_of_type_with_struct (in_v,s) env loc (x : Ast._type) = match x with
  | Ident i ->
    begin
      try
        List.assoc i type_keywords
      with Not_found ->
        if (is_struct env i) || (in_v && i = s) then
          Struct i
        else raise (Typing_error (loc, "Unknown type"))
    end
  | TypedIdent (i, t) ->
      if i = "Vec" then
        Vect (expr_type_of_type_with_struct (true,s) env loc t)
      else raise (Typing_error (loc, "Unknown bracket type"))
  | AddressOfMut t -> Ref (true, expr_type_of_type_with_struct (in_v,s) env loc t)
  | AddressOf t ->    Ref (false, expr_type_of_type_with_struct (in_v,s) env loc t)

let expr_type_of_type = expr_type_of_type_with_struct (false, "")

let expr_type_of_type_option env loc = function
  | None -> Unit
  | Some t -> expr_type_of_type env loc t

let rec check_type t a = match t, a with
  | Neutral, _ -> true
  | Ref (b1, e1), Ref (b2, e2) -> (b1 || (not b2)) && check_type e1 e2
  | Vect t1, Vect t2 -> check_type t1 t2
  | _, _ -> t = a

let rec update_vec_type t a = match t, a with
  | Neutral, _ -> a
  | Ref (b1, e1), Ref (b2, e2) -> Ref (b1 && b2, update_vec_type e1 e2)
  | _, _ -> a

let check_args loc (t_arg, ret) arg =
  let rec check_rec = function
    | ([], []) -> ret
    | (a::aq, t::tq) when check_type a t -> check_rec (aq,tq)
    | (_, _) -> raise (Typing_error (loc, "Incompatible types"))
  in check_rec (arg, t_arg)

let check_structure_instanciation (_, _, env) loc s decl =
  try
    let vars = Env.find s env in
    let fold_decl map = function
      | (id, _) when Env.mem id map ->
          raise (Typing_error (loc, "Duplicate variable in struct : " ^ id))
      | (id, _) when not (Env.mem id vars) ->
          raise (Typing_error (loc, "Unexpected variable in struct : " ^ id))
      | (id, t) when not (check_type t (Env.find id vars)) ->
          raise (Typing_error (loc, "Incompatible types for variable : " ^ id))
      | (id, t) -> Env.add id t map in
    let decl_map = List.fold_left fold_decl Env.empty decl in
    let check_presence id = function
      | _ when not (Env.mem id decl_map) ->
          raise (Typing_error (loc, "Missing variable in struct : " ^ id))
      | _ -> () in
    Env.iter check_presence vars
  with Not_found -> raise (Typing_error (loc, "Unknown structure : " ^ s))

(* All three following auxiliary functions :
  * raise Ret_fail if an unproperly-typed return can be hit
  * return true if a properly-typed return is always hit
  * return false if it is possible not to hit a return statement
 *)
exception Ret_fail of Ast.location
let check_function_return bloc ret =
  let rec check_instr = function
    | [] -> false
    | (TReturn (t, l, _))::_ ->
      begin
      match t with
      | None -> check_type Unit ret || raise (Ret_fail l)
      | Some u -> check_expr u ||
          check_type (type_of_expr u) ret ||
          raise (Ret_fail l)
      end
    | (TIf (c, t, e, _, y))::l ->
      check_expr c ||
      (check_bloc t && check_bloc e) ||
      check_instr l
    | (TWhile (c, b, _, _))::l ->
      check_expr c ||
      (check_bloc b && false) ||
      check_instr l
    | _::l -> check_instr l
  and check_expr = function
    | TUnop (_, e, _, _)
    | TDot (e, _, _, _)
    | TLen (e, _, _) -> check_expr e
    | TBinop (_, e1, e2, _, _)
    | TBrackets (e1, e2, _, _) -> check_expr e1 || check_expr e2
    | TFunCall (_, el, _, _)
    | TVec (el, _, _) -> List.exists check_expr el
    | TBloc (bloc, _, _) -> check_bloc bloc
    | _ -> false
  and check_bloc (instr, e, _, _) =
    check_instr instr ||
    match e with
      | None -> false
      | Some exp -> check_expr exp
  in
  let i, _, loc, ty = bloc in
  let correct_type = check_type ty ret
  and unit_type = check_type ty Unit in
  try
    if correct_type || unit_type then
      check_bloc bloc || correct_type
    else
      raise (Typing_error (loc, "Wrong type for bloc"))
  with Ret_fail l ->
    raise (Typing_error (l, "Invalid return type"))

let last l =
  if List.length l = 0 then raise (Failure "No last element in empty list");
  let rec aux el = function
    | [] -> el
    | t::q -> aux t q in
  aux (List.hd l) (List.tl l)

let rec is_l_value = function
  | TIdent (_, _, _) -> true
  | TUnop (Star, _, _, _) -> true
  | TBrackets (_, _, _, _) -> true
  | TDot (e, _, _, _) -> is_l_value e
  | _ -> false

let rec is_mut_value env = function
  | TIdent (i, _, _) -> is_mut env i
  | TUnop (Star, e, _, _) ->
      begin
        match type_of_expr e with
        | Ref (b, _) -> b
        | _ -> assert false
      end
  | TBrackets (e, _, _, _) -> is_mut_value env e
  | TDot (e, _, _, _) -> is_mut_value env e
  | _ -> false

let auto_deref loc te = match type_of_expr te with
  | Ref (_, Vect t) -> TUnop (Star, te, loc, Vect t)
  | Ref (_, Struct s) -> TUnop (Star, te, loc, Struct s)
  | _ -> te

let type_file file =
  let fold_env typer env =
    List.fold_left (fun (l, env) x ->
      let tx, n_env = typer env x in (tx::l, n_env))
    ([], env) in
  let rec type_decl env = function
    | DeclStruct ((i, i_loc), l, loc) ->
      if is_struct env i then
        raise (Typing_error (i_loc, "Redefinition of struct " ^ i))
      else
        check_duplicate_idents l;
        let n_arg = List.map
          (fun (x,l,y) ->
            (x, l, expr_type_of_type_with_struct (false,i) env l y))
          l in
        let n_env = decl_struct env i n_arg in
        TDeclStruct ((i, i_loc), n_arg, loc, Unit), n_env
    | DeclFun ((i, i_loc), l, t, b, loc) ->
      if is_fun env i then
        raise (Typing_error (i_loc, "Redefinition of function " ^ i))
      else
        check_duplicate_idents (List.map (fun (_,x,l,y) -> (x,l,y)) l);
        let ret_type = expr_type_of_type_option env loc t in
        let arg_type = List.map (fun (_,_,l,z) -> expr_type_of_type env l z) l in
        let n_env = decl_fun env i (arg_type, ret_type) in
        let n_arg = List.map (fun (x,y,l,z) -> (x, y, l, expr_type_of_type env loc z)) l in
        let n_env_args = List.fold_left
          (fun _env (b, x, _, t) -> decl_var _env x t b) n_env n_arg in
        let tb, _ = type_bloc n_env_args b in
        if check_function_return tb ret_type then
          TDeclFun ((i,i_loc), n_arg, ret_type, tb, loc, Unit), n_env
        else
          raise (Typing_error (i_loc, "Function "^i^" does not always return"))
  and type_expr env = function
    | Int (i, loc) -> TInt (i, loc, Int32), env
    | Bool (b, loc) -> TBool (b, loc, Boolean), env
    | Ident (i, loc) ->
        begin
          try TIdent (i, loc, var_type env i), env
          with Not_found -> raise (Typing_error (loc, "Undefined litteral : " ^ i))
        end
    | Unop (u, e, loc) ->
        begin
          let te, _ = type_expr env e in
          let r = match u with
          | Star -> begin
                    match type_of_expr te with
                    | Ref (_, t) -> t
                    | _ -> raise (Typing_error (loc, "Expected reference type"))
                    end
          | Amp ->
              if is_l_value te then
                Ref (false, type_of_expr te)
              else raise (Typing_error (loc, "Expected l_value for reference"))
          | AmpMut ->
              if not (is_l_value te) then
                raise (Typing_error (loc, "Expected l_value for mutable reference"))
              else if not (is_mut_value env te) then
                raise (Typing_error (loc, "Expected mutable value for mutable reference"))
              else
                Ref (true, type_of_expr te)
          | _ -> check_args loc (unop_type u) [(type_of_expr te)]
          in TUnop(u, te, loc, r), env
        end
    | Binop (b, e1, e2, loc) ->
        let te1, _ = type_expr env e1
        and te2, _ = type_expr env e2 in
        if b = Equal then
          let l = location_of_expr te1 in
          if not (type_of_expr te1 = type_of_expr te2) then
            raise (Typing_error (l, "Type mismatch in assignement"))
          else if not (is_l_value te1) then
            raise (Typing_error (l, "Expected l_value for assignement"))
          else if not (is_mut_value env te1) then
            raise (Typing_error (l, "Expected mutable value for assignement"))
          else
            TBinop (b, te1, te2, loc, Unit), env
        else
          let r = check_args loc (binop_type b) (List.map type_of_expr [te1; te2])
          in TBinop (b, te1, te2, loc, r), env
    | Dot (e, i, loc) ->
        let te = auto_deref loc (fst (type_expr env e)) in
        begin
          match type_of_expr te with
          | Struct s ->
              if not (is_struct env s) then
                raise (Typing_error (location_of_expr te, "Unknown structure "^s))
              else if not (has_variable env s i) then
                raise (Typing_error (loc, "Unknown variable "^i^" for struct "^s))
              else
                TDot (te, i, loc, struct_type env s i), env
          | _ -> raise (Typing_error (location_of_expr te,
                                      "Cannot call . on non-struct type"))
        end
    | Len (e, loc) ->
        let te = auto_deref loc (fst (type_expr env e)) in
        begin
          match type_of_expr te with
          | Vect t -> TLen (te, loc, Int32), env
          | _ -> raise (Typing_error (location_of_expr te,
                                      "Cannot call len on non-vec type"))
        end
    | Brackets (eo, ei, loc) ->
        let teo = auto_deref loc (fst (type_expr env eo)) in
        let tei = fst (type_expr env ei) in
        begin
          match type_of_expr teo with
          | Vect t -> if check_type (type_of_expr tei) Int32 then
                        TBrackets (teo, tei, loc, t), env
                      else
                        raise (Typing_error (location_of_expr tei,
                                            "Expected i32 argument to [] call"))
          | _ -> raise (Typing_error (location_of_expr teo,
                                     "Cannot call [] on non-vec type"))
        end
    | FunCall ((f,f_loc), arg, loc) ->
        if not (is_fun env f) then raise (Typing_error (f_loc, "Undefined function " ^ f)) else
        let targ = List.map (fun x -> fst (type_expr env x)) arg
        in let r = check_args loc (fun_type env f) (List.map type_of_expr targ)
        in TFunCall((f,f_loc), targ, loc, r), env
    | Vec (e, loc) ->
        let te = List.map (fun x -> fst (type_expr env x)) e in
        let types_list = List.map type_of_expr te in
        let t = List.fold_left update_vec_type Neutral types_list in
        if List.for_all (fun x -> check_type x t) types_list then
          TVec (te, loc, Vect t), env
        else raise (Typing_error (loc, "Incompatible vector elements"))
    | Print (s, loc) -> TPrint (s, loc, Unit), env
    | Bloc (b, loc) ->
        let tb, _ = type_bloc env b in
        TBloc (tb, loc, type_of_bloc tb), env
  and type_bloc env (instr_list, expr, loc) =
    let til, n_env = fold_env type_instr env instr_list in
    let te, r = match expr with
      | None ->
        let ty = if List.length til = 0 then Unit
          else match last til with
            | TIf (_, _, _, _, t) -> t
            | _ -> Unit
        in None, ty
      | Some e -> let t,_ = type_expr n_env e in Some t, type_of_expr t
    in (til, te, loc, r), env
  and type_instr env = function
    | Empty loc -> TEmpty (loc, Unit), env
    | Expr (e, loc) -> let te = fst (type_expr env e) in
        TExpr (te, loc, type_of_expr te), env
    | Let (b, (i,i_loc), e, loc) ->
        let te, _ = type_expr env e in
        let t = type_of_expr te in
        TLet(b, (i,i_loc), te, loc, Unit), (decl_var env i t b)
    | LetStruct (b, (i,i_loc), (s,s_loc), l, loc) ->
        let tl = List.map (fun (id, l, ex) -> (id, l, fst (type_expr env ex))) l in
        let env1 = decl_var env i (Struct s) b in
        let t = List.map (fun (id, _, te) -> (id, type_of_expr te)) tl in
        check_structure_instanciation env loc s t;
        TLetStruct (b, (i,i_loc), (s,s_loc), tl, loc, Unit), env1
    | While (e, b, loc) ->
        let te, _ = type_expr env e in
        if check_type (type_of_expr te) Boolean then
          begin
            let tb, _ = type_bloc env b in
            if check_type (type_of_bloc tb) Unit then
              TWhile (te, tb, loc, Unit), env
            else raise (Typing_error (loc, "Expected unit bloc as while argument"))
          end
        else raise (Typing_error (location_of_expr te,
                                 "Expected boolean condition for while bloc"))
    | Return (eo, loc) ->
        let teo = begin
          match eo with
          | None -> None
          | Some e -> Some (fst (type_expr env e))
        end in TReturn (teo, loc, Unit), env
    | If (e, b1, b2, loc) ->
        let te, _  = type_expr env e  in
        let tb1, _ = type_bloc env b1 in
        let tb2, _ = type_bloc env b2 in
        if (check_type (type_of_expr te) Boolean) then
          if (check_type (type_of_bloc tb1) (type_of_bloc tb2)) then
            let t = type_of_bloc tb1 in TIf (te, tb1, tb2, loc, t), env
          else raise (Typing_error (loc, "Expected same type for both blocs"))
        else raise (Typing_error (location_of_expr te,
                                 "Expected boolean condition for if bloc"))
  in let typed_file, _ = fold_env type_decl empty_env file
  in typed_file
