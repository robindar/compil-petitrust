open Ast
open Format

let print_binop fmt = function
  | Add -> fprintf fmt "+"
  | Sub -> fprintf fmt "-"
  | Mul -> fprintf fmt "*"
  | Div -> fprintf fmt "/"
  | Mod -> fprintf fmt "%%"

let print_unop fmt = function
  | Minus -> fprintf fmt "-"

let rec print fmt = function
  | Int i -> fprintf fmt "%d" i
  | Bool b -> fprintf fmt "%s" (if b then "True" else "False")
  | Ident i -> fprintf fmt "%s" i
  | Unop (op, e) -> fprintf fmt "(%a %a)" print_unop op print e
  | Binop (op, e1, e2) -> fprintf fmt "(%a %a %a)" print e1 print_binop op print e2

let buf = Lexing.from_channel stdin
let e = Parser.main Lexer.token buf

let () = printf "%a@." print e
