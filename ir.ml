open Ast

type ir_stmt =
  | IRSExpr of Ast.e
  | IRSVarAssign of string * Ast.e
  | IRSVarDecl of string * Ast.t

type ir_blockend =
  | IRSReturn of Ast.e option
  | IRSBranch of Ast.e * string * string
  | IRSJump of string

type ir_block = IRBlock of string * ir_stmt list * ir_blockend

type param = Ast.t * string

type ir_global = IRFunc of Ast.t * string * param list * ir_block