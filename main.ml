open Ast
open Format
open Lexing


let usage = "usage: prustc [options] file.rs"

let parse_only = ref false
let type_only  = ref false
let no_bc      = ref false
let no_asm     = ref false
let out_file = ref ""

let set_out_file s = out_file := s

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
    "--type-only",  Arg.Set type_only,  "  stop after typing";
    "--no-bc",      Arg.Set no_bc,      "  skip borrow-checking";
    "--no-asm",     Arg.Set no_asm,     "  stop after borrow checking";
    "-o", Arg.String set_out_file,  " <file>   specify output file name";
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

let report (b,e) =
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc

let () =
  let c = open_in file in
  let lb = Lexing.from_channel c in
  try
    let f = Parser.file Lexer.token lb in
    let () = close_in c in
    if !parse_only then exit 0 else
    let typed_file = Typer.type_file f in
    if !type_only then exit 0;
    Borrow_checker.borrow_check_file typed_file;
    if !no_asm then exit 0;
    if !out_file = "" then out_file := Filename.chop_suffix file ".rs" ^ ".s";
    Compiler.compile_program typed_file !out_file
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Typer.Typing_error (l, s) ->
        let f, t = l in report (f, t);
        eprintf "Typing error: %s@." s;
        exit 1
    | Borrow_checker.Borrow_checking_error (l, s) ->
        let f, t = l in report (f, t);
        eprintf "Borrow checking error: %s@." s;
        exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
