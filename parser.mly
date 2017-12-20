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
%token AND OR
%token EQ NEQ LEQ GEQ
%token DOT COMMA COLON SEMICOLON
%token EQUAL
%token IF ELSE
%token WHILE RETURN
%token BANG AMP
%token VEC PRINT
%right EQUAL
%left AND
%left OR
%nonassoc EQ NEQ RIGHTANGLE LEFTANGLE GEQ LEQ
%left PLUS MINUS
%left STAR SLASH PERCENT
%nonassoc UNARY_PREC
%nonassoc LEFTBRACKET
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
  | u = unary_op; e = expression %prec UNARY_PREC
    { Unop (u, e) }
  | LEFTPAREN; e = expression; RIGHTPAREN
    { e }
  | e = expression; DOT; i = IDENT
    { Dot (e, i) }
  | e = expression; DOT; LENGTH;
    { Len e }
  | e = expression; LEFTBRACKET; i = expression; RIGHTBRACKET
    { Brackets (e, i) }
  | i = IDENT; LEFTPAREN; l = separated_list(COMMA, expression); RIGHTPAREN
    { FunCall (i, l) }
  | VEC; LEFTBRACKET; l = separated_list(COMMA, expression); RIGHTBRACKET
    { Vec l }
  | PRINT; LEFTPAREN; s = STRING; RIGHTPAREN
    { Print s }
  | b = bloc
    { Bloc b }
%inline op:
  | STAR    { Mul }
  | SLASH   { Div }
  | PERCENT { Mod }
  | PLUS    { Add }
  | MINUS   { Sub }
  | AND     { And }
  | OR      { Or  }
  | EQ      { Eq  }
  | NEQ     { Neq }
  | LEQ     { Leq }
  | GEQ     { Geq }
  | EQUAL   { Equal }
  | LEFTANGLE  { Lt }
  | RIGHTANGLE { Gt }
%inline unary_op:
  | MINUS   { Minus  }
  | BANG    { Bang   }
  | STAR    { Star   }
  | AMP     { Amp    }
  | AMP MUT { AmpMut }
;

typ:
  i = IDENT
  { Ident i }
| i = IDENT; LEFTANGLE; t = typ; RIGHTANGLE
  { TypedIdent (i, t) }
| AMP; t = typ
  { AddressOf t }
| AMP; MUT; t = typ
  { AddressOfMut t }
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
| LET; m = mut; i = IDENT; EQUAL; i2 = IDENT; LEFTBRACE;
    l = separated_list(COMMA, a = IDENT; COLON; e = expression { (a, e) });
  RIGHTBRACE; SEMICOLON
  { LetStruct (m, i, i2, l) }
| RETURN; e = option(expression); SEMICOLON
  { Return e }
| WHILE; e = expression; b = bloc
  { While (e, b) }
| i = if_structure
  { i }

if_structure:
  IF; e = expression; b = bloc
  { If (e, b, ([], None)) }
| IF; e = expression; bi = bloc; ELSE be = bloc
  { If (e, bi, be) }
| IF; e = expression; bi = bloc; ELSE i = if_structure
  { If (e, bi, ([i], None)) }
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
