{
  open Lexing
  open Parser
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
  | ident as i { IDENT i }
  | '+'        { PLUS }
  | '-'        { MINUS }
  | '*'        { STAR }
  | '/'        { SLASH }
  | '%'        { PERCENT }
  | '('        { LEFTPAREN }
  | ')'        { RIGHTPAREN }
  | eof        { EOF }
