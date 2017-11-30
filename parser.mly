%{
  open Ast
%}
%token <int> INT
%token <string> IDENT
%token EOF TRUE FALSE
%token LEFTPAREN RIGHTPAREN
%token PLUS MINUS STAR SLASH PERCENT
%left PLUS MINUS
%left STAR SLASH PERCENT

%start main
%type <Ast.expr> main

%%

main:
  expression EOF { $1 }
;

expression:
  | INT                             { Int $1 }
  | TRUE                            { Bool true }
  | FALSE                           { Bool false }
  | IDENT                           { Ident $1 }
  | expression STAR expression      { Binop (Mul, $1, $3) }
  | expression SLASH expression     { Binop (Div, $1, $3) }
  | expression PERCENT expression   { Binop (Mod, $1, $3) }
  | expression PLUS expression      { Binop (Add, $1, $3) }
  | expression MINUS expression     { Binop (Sub, $1, $3) }
  | MINUS expression                { Unop (Minus, $2) }
  | LEFTPAREN expression RIGHTPAREN { $2 }
;
