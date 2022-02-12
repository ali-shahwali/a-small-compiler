open Ir
open Pprint

let pprintIrStmt s =
  match s with
  | IRSVarDecl(r, t) -> "IRSVarDecl(" ^ (Pprint.pprintIdent r) ^ "," ^ (Pprint.pprintType t) ^ ") \n"
  | IRSVarAssign(r, e) -> "IRSVarAssign(" ^ (Pprint.pprintIdent r) ^ "," ^ (Pprint.pprintExpr e) ^ ") \n"  


let rec pprintBlockStmts block =
  match block with
  | [] -> ""
  | (s::t) -> (pprintIrStmt s) ^ (pprintBlockStmts t)


let pprintBlockEnd blockEnd =
  match blockEnd with
  | IRSReturn(None) -> "IRSReturn()"
  | IRSReturn(Some(e)) -> "IRSReturn(" ^ (Pprint.pprintExpr e) ^ ")"
  | IRSBranch(e, r1, r2) -> "IRSBranch(" ^ (Pprint.pprintExpr e) ^ r1 ^ r2 ^ ")"
  | IRSJump(r) -> "IRSJump(" ^ r ^ ")"

let pprintGlobal g =
  match g with
  | IRFunc(t, r, p, IRBlock(label, stmts, blockEnd)) ->
     "IRFunc(" ^ (Pprint.pprintType t) ^ ", " ^ r ^ ", {" ^ (Pprint.pprintParams p) ^ "}, { \n IRBlock({" ^ label ^ ", \n" ^ (pprintBlockStmts stmts) ^ "\n" ^ (pprintBlockEnd blockEnd) ^ "}) \n })"

let rec pprint ir =
  match ir with
  | [] -> exit 0
  | (h::t) -> print_endline (pprintGlobal h); pprint t