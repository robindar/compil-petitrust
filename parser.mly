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
  e = expression; EOF { e }
;

expression:
  | i = INT
     { Int i }
  | TRUE
    { Bool true }
  | FALSE
    { Bool false }
  | s = IDENT
    { Ident s }
  | e1 = expression; STAR; e2 = expression
    { Binop (Mul, e1, e2) }
  | e1 = expression; SLASH; e2 = expression
    { Binop (Div, e1, e2) }
  | e1 = expression; PERCENT; e2 = expression
    { Binop (Mod, e1, e1) }
  | e1 = expression; PLUS; e2 = expression
    { Binop (Add, e1, e2) }
  | e1 = expression; MINUS; e2 = expression
    { Binop (Sub, e1, e1) }
  | MINUS; e = expression
    { Unop (Minus, e) }
  | LEFTPAREN; e = expression; RIGHTPAREN
    { e }
;
