open Typed_ast
open Precompiled_ast

module CompileEnv = Map.Make(String)

let empty_env = (0, CompileEnv.empty, 1)

(* size of type in bytes *)
let struct_data = ref CompileEnv.empty

let add_struct s data =
  struct_data := CompileEnv.add s data !struct_data

let get_struct_data s =
  CompileEnv.find s !struct_data
let get_struct_size s =
  fst (get_struct_data s)
let get_struct_vars s =
  snd (get_struct_data s)
let get_struct_var_offset s v =
  CompileEnv.find v (get_struct_vars s)

let size_of = function
  | Unit | Int32 | Boolean -> 1
  | Struct s -> get_struct_size s
  | _ -> assert false

let get_depth (d,_,_) = d

let get_var i (_,env,_) = CompileEnv.find i env
let get_next_offset (_,_,n) = n
let add_var i size (d, env, n) =
  (d, CompileEnv.add i (d,-(n + size - 1)) env, n + size)
let add_arg i ofs (d, env, n) =
  (d, CompileEnv.add i (d+1,ofs) env, n)
let dig (d, env, _) = (d + 1, env, 1)

let get_offset i env =
  let depth, ofs = get_var i env in
  let cur_depth = get_depth env in
  (cur_depth - depth, ofs)


let precompile p =
  let rec precompile_expr env = function
    | TBool (b,_,_) -> PBool b
    | TInt (i,_,_) -> PInt i
    | TIdent (i,_,t) ->
        PIdent (get_offset i env, t)
    | TUnop (op, e, _, t) ->
        PUnop (op, precompile_expr env e, t)
    | TBinop (Ast.Equal, e1, e2, _, _) ->
        PAssignement (precompile_expr env e1,
                      precompile_expr env e2,
                      Typer.type_of_expr e2)
    | TBinop (op, e1, e2, _, t) ->
        PBinop (op, precompile_expr env e1, precompile_expr env e2, t)
    | TDot (e, i, _, t) ->
        let s = match Typer.type_of_expr e with
          | Struct x -> x | _ -> assert false in
        PDot (precompile_expr env e, get_struct_var_offset s i, Struct s, t)
    | TLen (e, _, _) -> assert false
    | TBrackets (eo, ei, _, _) -> assert false
    | TFunCall ((f,_), el, _, t) ->
        let pel = List.map (precompile_expr env) el in
        let s = List.fold_left (+) 0
          (List.map (fun x -> size_of (Typer.type_of_expr x)) el) in
        PFunCall(f, pel, s, t)
    | TVec (el, _, _) -> assert false
    | TPrint (s, _, _) -> PPrint s
    | TBloc (b, _, _) -> PBloc (precompile_bloc env b)
  and precompile_decl env = function
    | TDeclStruct ((s,_), vars, _, t) ->
        let c = ref 0 in
        let process_var env (i, _, ty) =
          let n_env = CompileEnv.add i !c env in
          c := !c + (size_of ty);
          n_env in
        let s_data = List.fold_left process_var CompileEnv.empty vars in
        add_struct s (!c, s_data);
        PDeclStruct, env
    | TDeclFun ((f,_), arg, _, bloc, _, ty) ->
        let pos = ref 2 in
        let process_arg _env (_, i, _, t) =
          pos := !pos + (size_of t);
          add_arg i !pos _env in
        let _env = List.fold_left process_arg env arg in
        let pbloc = precompile_bloc _env bloc in
        PDeclFun (f, pbloc, !pos - 2, ty), env
  and precompile_bloc env (instr, expr, _, t) =
    let pinstr, _env = List.fold_left
      (fun (l,_env) i -> let pi, n_env = precompile_instr _env i in (pi::l, n_env))
      ([], dig env) instr in
    let vars_size = get_next_offset _env - 1 in
    match expr with
      | None -> (List.rev pinstr, None, vars_size, t)
      | Some e -> (List.rev pinstr, Some (precompile_expr _env e), vars_size, t)
  and precompile_instr env = function
    | TEmpty _ -> PEmpty, env
    | TExpr (e, _, t) -> PExpr (precompile_expr env e, t), env
    | TLet (_, (i,_), e, _, _) ->
        let t = Typer.type_of_expr e in
        let _env = (add_var i (size_of t) env) in
        PLet (get_var i _env, precompile_expr env e, t), _env
    | TLetStruct (_, (i, _), (s, _), vars, _, _) ->
        let t = Struct s in
        let compile_var (i, _, e) =
            (get_struct_var_offset s i,
             precompile_expr env e,
             size_of (Typer.type_of_expr e)) in
        let pvars = List.map compile_var vars in
        let _env = add_var i (size_of t) env in
        PLetStruct (get_var i _env, pvars, t), _env
    | TWhile (c, b, _, _) ->
        PWhile (precompile_expr env c, precompile_bloc env b), env
    | TReturn (eo, _, _) -> assert false
    | TIf (c, t, e, _, ty) ->
        PIf(precompile_expr env c,
            precompile_bloc env t,
            precompile_bloc env e,
            ty), env
  in let prec_file, _ = List.fold_left
      (fun (l,_env) d -> let pd, env = precompile_decl _env d in pd::l, env)
    ([], empty_env) p in
  prec_file
