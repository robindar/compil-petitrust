%{
  open Ast
%}
%token <int> INT
%token <string> STRING
%token <string> IDENT
%token EOF TRUE FALSE
%token LENGTH STRUCT FN ARROW MUT LET
%token LEFTPAREN RIGHTPAREN LEFTBRACKET RIGHTBRACKET LEFTBRACE RIGHTBRACE LEFTANGLE RIGHTANGLE
%token PLUS MINUS STAR SLASH PERCENT
%token DOT COMMA COLON SEMICOLON
%token EQUAL
%token WHILE RETURN
%token BANG AMP
%token VEC PRINT
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UMINUS_PREC
%nonassoc LEFTBRACKET
%nonassoc LENGTH
%nonassoc DOT

%start file
%type <Ast.typ> typ
%type <Ast.ident * Ast.typ> ident_typ
%type <Ast.decl> decl
%type <Ast.bloc> bloc
%type <Ast.file> file

%%

file:
  d = list(decl); EOF
  { d }
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
  | i = IDENT; LEFTPAREN; l = separated_list(COMMA, expression); RIGHTPAREN
    { Paren (i, l) }
  | VEC; BANG; LEFTBRACKET; l = separated_list(COMMA, expression); RIGHTBRACKET
    { Vec l }
  | PRINT; BANG; LEFTPAREN; s = STRING; RIGHTPAREN
    { Print s }
  | b = bloc
    { Bloc b }
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

mut:
  m = option(MUT)
  { (function | None -> false | _ -> true) m }
;

mut_ident_typ:
| m = mut; i = IDENT; COLON; t = typ
  { (m, i, t) }
;

decl:
  STRUCT; i = IDENT; LEFTBRACE; l = separated_list(COMMA, ident_typ) ; RIGHTBRACE
  { DeclStruct (i, l) }
| FN; i = IDENT; LEFTPAREN; l = separated_list(COMMA, mut_ident_typ); RIGHTPAREN; t = option(ARROW; s = typ { s }); b = bloc
  { DeclFun (i, l, t, b) }
;

instruction:
  SEMICOLON
  { Empty }
| e = expression; SEMICOLON
  { Expr e }
| LET; m = mut; i = IDENT; EQUAL; e = expression; SEMICOLON
  { Let (m, i, e) }
| LET; m = mut; i = IDENT; EQUAL; i2 = IDENT; LEFTBRACE; l = list(a = IDENT; COLON; e = expression { (a, e) }); RIGHTBRACE; SEMICOLON
  { LetStruct (m, i, i2, l) }
| RETURN; e = option(expression); SEMICOLON
  { Return e }
| WHILE; e = expression; b = bloc
  { While (e, b) }
;

bloc_body:
  i = instruction; b = bloc_body
  { let l, e = b in (i::l, e) }
| e = option(expression)
  { ([], e) }
;

bloc:
  LEFTBRACE; b = bloc_body; RIGHTBRACE
  { b }
;
