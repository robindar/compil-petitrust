open Ast

type expr_type =
  | Neutral
  | Unit
  | Int32
  | Boolean
  | Struct of ident
  | Vect of expr_type
  | Ref of bool * expr_type

type typed_expr =
  | TInt of int * expr_type
  | TBool of bool * expr_type
  | TIdent of ident * expr_type
  | TUnop of unop_type * typed_expr * expr_type
  | TBinop of binop_type * typed_expr * typed_expr * expr_type
  | TDot of typed_expr * ident * expr_type
  | TLen of typed_expr * expr_type
  | TBrackets of typed_expr * typed_expr * expr_type
  | TFunCall of ident * typed_expr list * expr_type
  | TVec of typed_expr list * expr_type
  | TPrint of _string * expr_type
  | TBloc of typed_bloc * expr_type
and typed_decl =
  | TDeclStruct of ident * (ident * expr_type) list * expr_type
  | TDeclFun of ident * (bool * ident * expr_type) list * expr_type * typed_bloc * expr_type
and typed_bloc = typed_instr list * typed_expr option * expr_type
and typed_instr =
  | TEmpty of expr_type
  | TExpr of typed_expr * expr_type
  | TLet of bool * ident * typed_expr * expr_type
  | TLetStruct of bool * ident * ident * (ident * typed_expr) list * expr_type
  | TWhile of typed_expr * typed_bloc * expr_type
  | TReturn of typed_expr option * expr_type
  | TIf of typed_expr * typed_bloc * typed_bloc * expr_type

type typed_file = typed_decl list
