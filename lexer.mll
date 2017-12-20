{
  open Lexing
  open Parser

  exception Lexing_error of string

  let ident_or_keyword =
    let populate table = (fun (kw, token) -> Hashtbl.add table kw token) in
    let reserved_keywords = Hashtbl.create 32 in
    List.iter (populate reserved_keywords)
      ["true", TRUE; "false", FALSE;
       "let", LET; "struct", STRUCT; "fn", FN;
       "if", IF; "else", ELSE; "while", WHILE;
       "mut", MUT; "return", RETURN];
    function str -> try Hashtbl.find reserved_keywords str with _ -> IDENT str

}

let whitespace = [' ' '\t']
let newline = '\n'
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let _char = [^ '"' '\\' ] | ( '\\' '"') | ('\\' '\\') | ('\\' 'n')
let _string = '"' _char * '"'

let length = "len" whitespace* "(" whitespace* ")"
let print = "print" whitespace* "!"
let vec = "vec" whitespace* "!"

let single_line_comment = "//" [^ '\n' ] *

let ident = alpha ( alpha | digit | '_' ) *

rule token = parse
  whitespace+ { token lexbuf }
  | newline    { new_line lexbuf; token lexbuf }
  | single_line_comment { token lexbuf }
  | "/*"       { comment 1 lexbuf }
  | digit + as d { INT (int_of_string d) }
  | _string as s { STRING s }
  | length     { LENGTH }
  | print      { PRINT }
  | vec        { VEC }
  | ident as i { ident_or_keyword i }
  | "->"       { ARROW }
  | "&&"       { AND }
  | "||"       { OR }
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
  | newline    { new_line lexbuf; comment n lexbuf }
  | _  { comment n lexbuf }
