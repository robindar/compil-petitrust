%{
  open Ast

  let process_string s =
    let t = String.sub s 1 (String.length s - 2) in
    Scanf.unescaped t
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
%type <Ast._type> _type
%type <Ast.ident * Ast.location * Ast._type> ident_type
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
     { Int (i, ($startpos, $endpos)) }
  | TRUE
    { Bool (true, ($startpos, $endpos)) }
  | FALSE
    { Bool (false, ($startpos, $endpos)) }
  | s = IDENT
    { Ident (s, ($startpos, $endpos)) }
  | e1 = expression; o = op; e2 = expression
    { Binop (o, e1, e2, ($startpos, $endpos)) }
  | u = unary_op; e = expression %prec UNARY_PREC
    { Unop (u, e, ($startpos, $endpos)) }
  | LEFTPAREN; e = expression; RIGHTPAREN
    { e }
  | e = expression; DOT; i = IDENT
    { Dot (e, i, ($startpos, $endpos)) }
  | e = expression; DOT; LENGTH;
    { Len (e, ($startpos, $endpos)) }
  | e = expression; LEFTBRACKET; i = expression; RIGHTBRACKET
    { Brackets (e, i, ($startpos, $endpos)) }
  | i = IDENT; LEFTPAREN; l = separated_list(COMMA, expression); RIGHTPAREN
    { FunCall ((i, ($startpos(i), $endpos(i))), l, ($startpos, $endpos)) }
  | VEC; LEFTBRACKET; l = separated_list(COMMA, expression); RIGHTBRACKET
    { Vec (l, ($startpos, $endpos)) }
  | PRINT; LEFTPAREN; s = STRING; RIGHTPAREN
    {Print (process_string s, ($startpos, $endpos)) }
  | b = bloc
    { Bloc (b, ($startpos, $endpos)) }
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

_type:
  i = IDENT
  { Ident i }
| i = IDENT; LEFTANGLE; t = _type; RIGHTANGLE
  { TypedIdent (i, t) }
| AMP; t = _type
  { AddressOf t }
| AMP; MUT; t = _type
  { AddressOfMut t }
;

ident_type:
  i = IDENT; COLON; t = _type
  { (i, ($startpos, $endpos), t) }
;

mut:
  m = option(MUT)
  { (function | None -> false | _ -> true) m }
;

mut_ident_type:
| m = mut; i = IDENT; COLON; t = _type
  { (m, i, ($startpos(i), $endpos(i)), t) }
;

decl:
  STRUCT; i = IDENT; LEFTBRACE; l = separated_list(COMMA, ident_type) ; RIGHTBRACE
  { DeclStruct ((i, ($startpos(i), $endpos(i))), l, ($startpos, $endpos)) }
| FN; i = IDENT; LEFTPAREN; l = separated_list(COMMA, mut_ident_type); RIGHTPAREN; t = option(ARROW; s = _type { s }); b = bloc
  { DeclFun ((i, ($startpos(i), $endpos(i))), l, t, b, ($startpos, $endpos)) }
;

struct_arg:
  a = IDENT; COLON; e = expression
  { (a, ($startpos(a), $endpos(a)), e) }

instruction:
  SEMICOLON
  { Empty ($startpos, $endpos) }
| e = expression; SEMICOLON
  { Expr (e, ($startpos, $endpos)) }
| LET; m = mut; i = IDENT; EQUAL; e = expression; SEMICOLON
  { Let (m, (i, ($startpos(i), $endpos(i))), e, ($startpos, $endpos)) }
| LET; m = mut; i1 = IDENT; EQUAL; i2 = IDENT; LEFTBRACE;
    l = separated_list(COMMA, struct_arg);
  RIGHTBRACE; SEMICOLON
  { LetStruct (m, (i1, ($startpos(i1), $endpos(i1))),
                  (i2, ($startpos(i2), $endpos(i2))),
                  l, ($startpos, $endpos)) }
| RETURN; e = option(expression); SEMICOLON
  { Return (e, ($startpos, $endpos)) }
| WHILE; e = expression; b = bloc
  { While (e, b, ($startpos, $endpos)) }
| i = if_structure
  { i }

if_structure:
  IF; e = expression; b = bloc
  { If (e, b, ([], None, ($startpos, $endpos)), ($startpos, $endpos)) }
| IF; e = expression; bi = bloc; ELSE be = bloc
  { If (e, bi, be, ($startpos, $endpos)) }
| IF; e = expression; bi = bloc; ELSE i = if_structure
  { If (e, bi, ([i], None, ($startpos, $endpos)), ($startpos, $endpos)) }
;

bloc_body:
  i = instruction; b = bloc_body
  { let l, e = b in (i::l, e) }
| e = option(expression)
  { ([], e) }
;

bloc:
  LEFTBRACE; b = bloc_body; RIGHTBRACE
  { let l, e = b in (l, e, ($startpos, $endpos)) }
;
