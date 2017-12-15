{
  open Lexing
  open Parser

  exception Lexing_error of string
}

let whitespace = [' ' '\t' '\n'] +
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let _char = [^ '"' '\\' ] | ('\\' '\\') | ('\\' 'n')
let _string = '"' _char * '"'

let single_line_comment = "//" [^ '\n' ] *

let ident = alpha ( alpha | digit | '_' ) *

rule token = parse
  whitespace { token lexbuf }
  | single_line_comment { token lexbuf }
  | "/*"       { comment 1 lexbuf }
  | digit + as d { INT (int_of_string d) }
  | _string as s { STRING s }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | "len"      { LENGTH }
  | "struct"   { STRUCT }
  | "fn"       { FN }
  | "mut"      { MUT }
  | "let"      { LET }
  | "while"    { WHILE }
  | "return"   { RETURN }
  | "vec"      { VEC }
  | "print"    { PRINT }
  | "->"       { ARROW }
  | ident as i { IDENT i }
  | "=="       { EQ }
  | "!="       { NEQ }
  | "<="       { LEQ }
  | ">="       { GEQ }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { STAR }
  | '/'        { SLASH }
  | '%'        { PERCENT }
  | '('        { LEFTPAREN }
  | ')'        { RIGHTPAREN }
  | '['        { LEFTBRACKET }
  | ']'        { RIGHTBRACKET }
  | '{'        { LEFTBRACE }
  | '}'        { RIGHTBRACE }
  | '<'        { LEFTANGLE }
  | '>'        { RIGHTANGLE }
  | '.'        { DOT }
  | ','        { COMMA }
  | ':'        { COLON }
  | ';'        { SEMICOLON }
  | '='        { EQUAL }
  | '!'        { BANG }
  | '&'        { AMP }
  | eof        { EOF }
  | _ as c     { raise
                  (Lexing_error
                    ("illegal character: " ^ String.make 1 c)
                  ) }
and comment n = parse
  | "/*"       { comment (n+1) lexbuf }
  | "*/"       { if n = 1 then token lexbuf else comment (n-1) lexbuf }
  | eof        { raise (Lexing_error "Missing terminating \"*/\" in comment") }
  | _  { comment n lexbuf }
