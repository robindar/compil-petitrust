type binop_type = Add | Sub | Mul | Div | Mod
  | Equal | Eq | Neq | Leq | Geq | Lt | Gt | And | Or
type unop_type  = Minus | Bang | Star | Amp | AmpMut

type ident = string
type _string = string

type location = Lexing.position * Lexing.position

type _type=
  | Ident of ident
  | TypedIdent of ident * _type
  | AddressOf of _type
  | AddressOfMut of _type

type expr =
  | Int of int * location
  | Bool of bool * location
  | Ident of ident * location
  | Unop of unop_type * expr * location
  | Binop of binop_type * expr * expr * location
  | Dot of expr * ident * location
  | Len of expr * location
  | Brackets of expr * expr * location
  | FunCall of ident * expr list * location
  | Vec of expr list * location
  | Print of _string * location
  | Bloc of bloc * location
and decl =
  | DeclStruct of ident * (ident * _type) list * location
  | DeclFun of ident * (bool * ident * _type) list * _type option * bloc * location
and bloc = instr list * expr option * location
and instr =
  | Empty of location
  | Expr of expr * location
  | Let of bool * ident * expr * location
  | LetStruct of bool * ident * ident * (ident * expr) list * location
  | While of expr * bloc * location
  | Return of expr option * location
  | If of expr * bloc * bloc * location

type file = decl list
