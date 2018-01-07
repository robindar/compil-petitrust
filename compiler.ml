open Format
open X86_64
open Typed_ast

let lib =
  label "print" ++
  movq (imm 0) (reg rax) ++
  call "printf" ++
  ret


let compile_decl = function
  | TDeclStruct _ -> assert false
  | TDeclFun _ -> assert false

let compile_program p out_file =
  (* let code = List.fold_left (++) nop (List.map compile_decl p) in *)
  let code =
    label "_main" ++
    movq (ilab ".hello_world") (reg rdi) ++
    call "print" ++
    ret in
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
      data = (label ".hello_world" ++ string "hello world!\n");
    } in
  let f = open_out out_file in
  let fmt = formatter_of_out_channel f in
  X86_64.print_program fmt p;
  fprintf fmt "@?";
  close_out f
