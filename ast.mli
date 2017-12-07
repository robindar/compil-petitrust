type binop_type = Add | Sub | Mul | Div | Mod
type unop_type  = Minus

type ident = string

type expr =
  | Int of int
  | Bool of bool
  | Ident of ident
  | Unop of unop_type * expr
  | Binop of binop_type * expr * expr
  | Dot of expr * ident
  | Len of expr
  | Brackets of expr * expr

type typ = ident

type decl =
  | DeclStruct of ident * (ident * typ) list
