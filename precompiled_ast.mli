open Ast
open Typed_ast

type offset = int * int

type prec_expr =
  | PInt of int
  | PBool of bool
  | PIdent of offset * expr_type
  | PUnop of unop_type * prec_expr * expr_type
  | PBinop of binop_type * prec_expr * prec_expr * expr_type
  | PDot of prec_expr * offset * expr_type
  | PLen of prec_expr * expr_type
  | PBrackets of prec_expr * prec_expr * expr_type
  | PFunCall of ident * prec_expr list * int * expr_type
  | PVec of prec_expr list * expr_type
  | PPrint of _string
  | PBloc of prec_bloc
and prec_decl =
  | PDeclStruct
  | PDeclFun of ident * prec_bloc * int * expr_type
and prec_bloc = prec_instr list * prec_expr option * int * expr_type
and prec_instr =
  | PEmpty
  | PExpr of prec_expr * expr_type
  | PLet of offset * prec_expr * expr_type
  | PLetStruct of offset * (int * prec_expr) list * expr_type
  | PWhile of prec_bloc
  | PReturn of prec_expr option
  | PIf of prec_expr * prec_bloc * prec_bloc * expr_type

type typed_file = typed_decl list
