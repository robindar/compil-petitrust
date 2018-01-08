open Format
open X86_64
open Ast
open Precompiled_ast

let int_of_bool b = assert false

let data_count = ref (-1)
let data_seg = ref nop
let register_data str =
  data_count := !data_count + 1;
  let id = "_str_" ^ (string_of_int !data_count) in
  data_seg := label id ++ string str ++ !data_seg;
  id

let memmove (f,fr) (t,tr) s =
  let rec _mov = function
    | -1 -> nop
    | i  -> _mov (i-1) ++
      movq (ind ~ofs:((f+i)*8) fr) (reg r15) ++
      movq (reg r15) (ind ~ofs:((t+i)*8) tr) in
  _mov (s-1)

(* extended push *)
let epush (ofs,r) size =
  memmove (ofs,r) (0,rsp) size ++
  subq (imm (size * 8)) (reg rsp)

let size_of = Precompiler.size_of

let lib =
  label "_print" ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  ret

let rec compile_expr = function
  | PInt i ->
      pushq (imm i)
  | PBool b ->
      pushq (imm (int_of_bool b))
  | PIdent ((_,ofs), ty) ->
      epush (ofs,rbp) (size_of ty)
  | PUnop (op, e, _) ->
      compile_expr e ++
      begin match op with
      | Minus ->
          popq rbx ++
          movq (imm 0) (reg rax) ++
          subq (reg rbx) (reg rax) ++
          pushq (reg rax)
      | Bang -> assert false
      | Star | Amp | AmpMut -> assert false
      end
  | PBinop (Equal, e1, e2, _) ->
      assert false
  | PBinop (op, e1, e2, _) ->
      compile_expr e1 ++
      compile_expr e2 ++
      popq rbx ++ popq rax ++
      begin match op with
      | Add -> addq (reg rbx) (reg rax)
      | Sub -> subq (reg rbx) (reg rax)
      | Mul -> imulq (reg rbx) (reg rax)
      | Div -> cqto ++ idivq (reg rbx)
      | Mod -> assert false
      | Eq  -> assert false
      | Neq -> assert false
      | Geq -> assert false
      | Leq -> assert false
      | Gt  -> assert false
      | Lt  -> assert false
      | And -> assert false
      | Or  -> assert false
      | Equal -> assert false
      end ++ pushq (reg rax)
  | PPrint s ->
      let id = register_data s in
      movq (ilab id) (reg rdi) ++
      call "_print"
  | _ -> assert false
and compile_bloc (instr, expr, _) =
  List.fold_left (++) nop (List.map compile_instr instr) ++
  match expr with
    | None -> nop
    | Some e -> compile_expr e
and compile_instr = function
  | PEmpty -> nop
  | PExpr (e,_) -> compile_expr e ++ popq rax
  | PLet ((_,i), e, t) ->
      let t = size_of t in
      compile_expr e ++
      memmove (t, rbp) (i, rbp) t
  | _ -> assert false

let compile_decl = function
  | PDeclStruct _ -> assert false
  | PDeclFun (f, bloc) ->
      label f ++
      compile_bloc bloc ++
      ret

let compile_program p out_file =
  let p = Precompiler.precompile p in
  let code = List.fold_left (++) nop (List.map compile_decl p) in
  let p =
    { text =
        globl "main" ++ label "main" ++
        (* initialize *)
        movq (reg rsp) (reg rbp) ++
        (* main code *)
        (call "_main") ++
        (* exit 0 *)
        movq (imm 0) (reg rax) ++
        ret ++
        (* external calls *)
        lib ++
        (* functions code *)
        code;
      data = !data_seg;
    } in
  let f = open_out out_file in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
