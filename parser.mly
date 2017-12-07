%{
  open Ast
%}
%token <int> INT
%token <string> IDENT
%token EOF TRUE FALSE
%token LENGTH STRUCT
%token LEFTPAREN RIGHTPAREN LEFTBRACKET RIGHTBRACKET LEFTBRACE RIGHTBRACE
%token PLUS MINUS STAR SLASH PERCENT
%token DOT COMMA COLON
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS_PREC
%nonassoc LEFTBRACKET
%nonassoc LENGTH
%nonassoc DOT

%start main
%type <Ast.typ> typ
%type <Ast.ident * Ast.typ> ident_typ
%type <Ast.decl> main

%%

main:
  e = decl_struct; EOF { e }
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
  | e1 = expression; o = op; e2 = expression
    { Binop (o, e1, e2) }
  | MINUS; e = expression %prec UMINUS_PREC
    { Unop (Minus, e) }
  | LEFTPAREN; e = expression; RIGHTPAREN
    { e }
  | e = expression; DOT; i = IDENT
    { Dot (e, i) }
  | e = expression; LENGTH
    { Len e }
  | e = expression; LEFTBRACKET; i = expression; RIGHTBRACKET
    { Brackets (e, i) }
%inline op:
  | STAR    { Mul }
  | SLASH   { Div }
  | PERCENT { Mod }
  | PLUS    { Add }
  | MINUS   { Sub }
;

typ:
  i = IDENT
  { i }
;

ident_typ:
  i = IDENT; COLON; t = typ
  { (i, t) }
;

decl_struct:
  STRUCT; i = IDENT; LEFTBRACE; l = separated_list(COMMA, ident_typ) ; RIGHTBRACE
  { DeclStruct (i, l) }
;
