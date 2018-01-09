open Format
open X86_64
open Ast
open Precompiled_ast

let int_of_bool b = if b then 1 else 0

let data_count = ref (-1)
let data_seg = ref nop
let register_data str =
  data_count := !data_count + 1;
  let id = "_str_" ^ (string_of_int !data_count) in
  data_seg := label id ++ string str ++ !data_seg;
  id

let if_count = ref (-1)
let register_if () =
  if_count := !if_count + 1;
  let s = string_of_int !if_count in
  ("_else_" ^ s, "_end_" ^ s)

let pushn size =
  subq (imm (size * 8)) (reg rsp)
let popn size =
  addq (imm (size * 8)) (reg rsp)

let memmove (f,fr) (t,tr) s =
  movq (imm s) (reg rdx) ++
  leaq (ind ~ofs:(8 * f) fr) rsi ++
  leaq (ind ~ofs:(8 * t) tr) rdi ++
  call "_memmove"

(* extended push *)
let epush (f,fr) size =
  pushn size ++
  memmove (f,fr) (size-1,rsp) size

let size_of = Precompiler.size_of

(* sets rdi to rbp d levels above *)
let rec reach_depth = function
  | 0 -> movq (reg rbp) (reg rdi)
  | d -> reach_depth (d-1) ++
    movq (ind rdi) (reg rdi)

let lib =
  (* rdi : string label *)
  label "_print" ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  ret ++

  (* rdi : destination address
   * rsi : source address
   * rdx : size to copy *)
  label "_memmove" ++
  xorq (reg rcx) (reg rcx) ++
  label "_memmove_loop" ++
  movq (ind ~index:rcx ~scale:8 rsi) (reg r15) ++
  movq (reg r15) (ind ~index:rcx ~scale:8 rdi) ++
  incq (reg rcx) ++
  cmpq (reg rcx) (reg rdx) ++
  jl "_memmove_loop" ++
  ret


let rec compile_expr = function
  | PInt i ->
      pushq (imm i)
  | PBool b ->
      pushq (imm (int_of_bool b))
  | PIdent ((d,ofs), ty) ->
      reach_depth d ++
      epush (ofs,rdi) (size_of ty)
  | PUnop (op, e, _) ->
      compile_expr e ++
      begin match op with
      | Minus ->
          popq rbx ++
          movq (imm 0) (reg rax) ++
          subq (reg rbx) (reg rax) ++
          pushq (reg rax)
      | Bang ->
          popq rax ++
          movq (imm 0) (reg r9) ++
          testq (reg rax) (reg rax) ++
          sete (reg r9b) ++
          pushq (reg r9)
      | Star | Amp | AmpMut -> assert false
      end
  | PBinop (Equal, e1, e2, _) ->
      assert false
  | PBinop (op, e1, e2, _) ->
      let compare setter =
        movq (imm 0) (reg r9) ++
        cmpq (reg rbx) (reg rax) ++
        setter (reg r9b) ++
        movq (reg r9) (reg rax) in
      compile_expr e1 ++
      compile_expr e2 ++
      popq rbx ++ popq rax ++
      begin match op with
      | Add -> addq (reg rbx) (reg rax)
      | Sub -> subq (reg rbx) (reg rax)
      | Mul -> imulq (reg rbx) (reg rax)
      | Div -> cqto ++ idivq (reg rbx)
      | Mod ->
          cqto ++ idivq (reg rbx) ++
          movq (reg rdx) (reg rax)
      | Eq  -> compare sete
      | Neq -> compare setne
      | Geq -> compare setge
      | Leq -> compare setle
      | Gt  -> compare setg
      | Lt  -> compare setl
      | And -> andq (reg rbx) (reg rax)
      | Or  -> orq (reg rbx) (reg rax)
      | Equal -> assert false
      end ++ pushq (reg rax)
  | PDot (e, (d,o), t) -> assert false
  | PLen (e, t) -> assert false
  | PBrackets (eo, ei, t) -> assert false
  | PFunCall (f, el, arg_size, t) ->
      List.fold_left (++) nop (List.rev (List.map compile_expr el)) ++
      call f ++
      popn arg_size ++
      epush (0,rax) (size_of t)
  | PPrint s ->
      let id = register_data s in
      movq (ilab id) (reg rdi) ++
      call "_print" ++
      pushq (imm 0)
  | PBloc b -> compile_bloc b
  | _ -> assert false
and compile_bloc (instr, expr, vars_size, t) =
  pushq (reg rbp) ++
  movq (reg rsp) (reg rbp) ++
  (if vars_size > 0 then pushn vars_size else nop) ++
  List.fold_left (++) nop (List.map compile_instr instr) ++
  begin match expr with
    | None -> pushq (imm 0)
    | Some e -> compile_expr e
  end ++
  movq (reg rsp) (reg rax) ++
  subq (imm (8 * (size_of t - 1))) (reg rax) ++
  popn (size_of t) ++
  (if vars_size > 0 then
    popn vars_size
  else nop) ++
  popq rbp

and compile_instr = function
  | PEmpty -> nop
  | PExpr (e,t) ->
      compile_expr e ++
      popn (size_of t)
  | PLet ((_,i), e, t) ->
      let t = size_of t in
      compile_expr e ++
      memmove (t-1,rsp) (i,rbp) t ++
      popn t
  | PLetStruct _ -> assert false
  | PWhile _ -> assert false
  | PIf (c, t, e, ty) ->
      let _else, _end = register_if () in
      compile_expr c ++
      popq rax ++
      testq (reg rax) (reg rax) ++
      jz _else ++
      compile_bloc t ++
      jmp _end ++
      label _else ++
      compile_bloc e ++
      label _end
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
