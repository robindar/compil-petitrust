%token PLUS
%token <int> INT
%token EOF
%left PLUS

%start main
%type <Ast.expr> main

%%

main:
  expression EOF { $1 }
;

expression:
  | INT
    { Int $1 }
  | expression PLUS expression
    { Add ($1, $3) }
;
