type binop_type = Add | Sub | Mul | Div | Mod
  | Equal | Eq | Neq | Leq | Geq | Lt | Gt | And | Or
type unop_type  = Minus | Bang | Star | Amp | AmpMut

type ident = string
type _string = string

type typ =
  | Ident of ident
  | TypedIdent of ident * typ
  | AddressOf of typ
  | AddressOfMut of typ

type expr =
  | Int of int
  | Bool of bool
  | Ident of ident
  | Unop of unop_type * expr
  | Binop of binop_type * expr * expr
  | Dot of expr * ident
  | Len of expr
  | Brackets of expr * expr
  | FunCall of ident * expr list
  | Vec of expr list
  | Print of _string
  | Bloc of bloc
and decl =
  | DeclStruct of ident * (ident * typ) list
  | DeclFun of ident * (bool * ident * typ) list * typ option * bloc
and bloc = instr list * expr option
and instr =
  | Empty
  | Expr of expr
  | Let of bool * ident * expr
  | LetStruct of bool * ident * ident * (ident * expr) list
  | While of expr * bloc
  | Return of expr option
  | If of expr * bloc * bloc

type file = decl list
