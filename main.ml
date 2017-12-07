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


let usage = "usage: prustc [options] file.rs"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".rs") then
      raise (Arg.Bad "no .rs extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1


let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    close_in c;
    if !parse_only then exit 0;
    (* Move on *)
  with
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
