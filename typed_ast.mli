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
  | TInt of int * location * expr_type
  | TBool of bool * location * expr_type
  | TIdent of ident * location * expr_type
  | TUnop of unop_type * typed_expr * location * expr_type
  | TBinop of binop_type * typed_expr * typed_expr * location * expr_type
  | TDot of typed_expr * ident * location * expr_type
  | TLen of typed_expr * location * expr_type
  | TBrackets of typed_expr * typed_expr * location * expr_type
  | TFunCall of (ident * location) * typed_expr list * location * expr_type
  | TVec of typed_expr list * location * expr_type
  | TPrint of _string * location * expr_type
  | TBloc of typed_bloc * location * expr_type
and typed_decl =
  | TDeclStruct of (ident * location)
                   * (ident * location * expr_type) list
                   * location * expr_type
  | TDeclFun of (ident * location)
                * (bool * ident * location * expr_type) list
                * expr_type * typed_bloc
                * location * expr_type
and typed_bloc = typed_instr list * typed_expr option * location * expr_type
and typed_instr =
  | TEmpty of location * expr_type
  | TExpr of typed_expr * location * expr_type
  | TLet of bool * (ident * location) * typed_expr * location * expr_type
  | TLetStruct of bool * (ident * location) * (ident * location)
                  * (ident * location * typed_expr) list * location * expr_type
  | TWhile of typed_expr * typed_bloc * location * expr_type
  | TReturn of typed_expr option * location * expr_type
  | TIf of typed_expr * typed_bloc * typed_bloc * location * expr_type

type typed_file = typed_decl list
