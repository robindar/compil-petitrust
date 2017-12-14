{
  open Lexing
  open Parser

  exception Lexing_error of string
}

let whitespace = [' ' '\t' '\n'] +
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let ident = alpha ( alpha | digit | '_' ) *

rule token = parse
  whitespace { token lexbuf }
  | digit + as d { INT (int_of_string d) }
  | "true"     { TRUE }
  | "false"    { FALSE }
  | ".len()"   { LENGTH }
  | "struct"   { STRUCT }
  | "fn"       { FN }
  | "mut"      { MUT }
  | "let"      { LET }
  | "while"    { WHILE }
  | "return"   { RETURN }
  | "->"       { ARROW }
  | ident as i { IDENT i }
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
  | '.'        { DOT }
  | ','        { COMMA }
  | ':'        { COLON }
  | '='        { EQUAL }
  | eof        { EOF }
  | _ as c     { raise
                  (Lexing_error
                    ("illegal character: " ^ String.make 1 c)
                  ) }
