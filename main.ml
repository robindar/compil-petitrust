open Ast
open Format

let rec print fmt = function
  | Int i -> fprintf fmt "%d" i
  | Add (e1, e2) -> fprintf fmt "(%a + %a)" print e1 print e2

let buf = Lexing.from_channel stdin
let e = Parser.main Lexer.token buf

let () = printf "%a@." print e
