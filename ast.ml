type uop =
  | Not
  | Comp
  | UMinus

type bop =
  | Plus
  | Minus
  | Mult
  | Div
  | Mod
  | Less
  | Greater
  | LessEq
  | GreaterEq
  | BShiftL
  | BShiftR
  | Eq
  | Neq
  | BWAnd
  | BWOr
  | And
  | Or

type t =
  | TInt
  | TVoid
  | TChar
  | TIdent of string
  | TPoint of t

type e = 
  | EInt of int
  | EChar of char
  | EString of string
  | EVar of string
  | EBinOp of bop * e * e
  | ECall of string * (e list)
  | ENew of t * e
  | EUnOp of uop * e
  | EArrayAccess of string * e * (string option) 

type s =
  | SExpr of e
  | SVarDef of t * string * e
  | SVarAssign of string * e
  | SScope of (s list)
  | SIf of e * s * (s option)
  | SWhile of e * s
  | SBreak
  | SReturn of (e option)
  | SDelete of string
  | SArrayAssign of string * e * (string option) * e

type g =
  | GVarDef of t * string * e
  | GVarDecl of t * string
  | GFuncDecl of t * string * ((t * string) list)
  | GFuncDef of t * string * ((t * string) list) * s
  | GStruct of string * ((t * string) list)

