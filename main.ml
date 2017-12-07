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

let rec print_expr fmt = function
  | Int i -> fprintf fmt "%d" i
  | Bool b -> fprintf fmt "%s" (if b then "True" else "False")
  | Ident i -> fprintf fmt "%s" i
  | Unop (op, e) -> fprintf fmt "(%a %a)" print_unop op print_expr e
  | Binop (op, e1, e2) -> fprintf fmt "(%a %a %a)" print_expr e1 print_binop op print_expr e2
  | Dot (e, s) -> fprintf fmt "(%a) . %s" print_expr e s
  | Len e -> fprintf fmt "(%a) .len()" print_expr e
  | Brackets (e, i) -> fprintf fmt "(%a) [%a]" print_expr e print_expr i

let buf = Lexing.from_channel stdin
let e = Parser.main Lexer.token buf

(* let () = printf "%a@." print e *)
