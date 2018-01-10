open Typed_ast
open Precompiled_ast

module CompileEnv = Map.Make(String)

let empty_env = (0, CompileEnv.empty, 1)

(* size of type in bytes *)
let size_of = function
  | Unit | Int32 | Boolean -> 1
  | _ -> assert false

let get_depth (d,_,_) = d

let get_var i (_,env,_) = CompileEnv.find i env
let get_next_offset (_,_,n) = n
let add_var i size (d, env, n) =
  (d, CompileEnv.add i (d,-n) env, n + size)
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
    | TBinop (op, e1, e2, _, t) ->
        PBinop (op, precompile_expr env e1, precompile_expr env e2, t)
    | TDot (e, i, _, _) -> assert false
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
    | TDeclStruct _ -> assert false
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
    | TLet (_, (i,_), e, _, t) ->
        let _env = (add_var i (size_of t) env) in
        PLet (get_var i _env, precompile_expr env e, t), _env
    | TLetStruct (_, (i, _), (s, _), vars, _, _) -> assert false
    | TWhile (c, b, _, _) -> assert false
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
