type binop_type = Add | Sub | Mul | Div | Mod
type unop_type  = Minus

type expr =
  | Int of int
  | Bool of bool
  | Ident of string
  | Unop of unop_type * expr
  | Binop of binop_type * expr * expr
  | Dot of expr * string
  | Len of expr
  | Brackets of expr * expr
