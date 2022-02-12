open Ast
open Ir

let rec genIrBlockEnd stmts =
  match stmts with
  | (SReturn(e)::[]) -> IRSReturn(e) 
  | (_::t) ->  genIrBlockEnd t

let rec genIrStmts stmts acc = 
  match stmts with
  | h::[] -> List.rev acc
  | SVarDef(t, r, e)::rest -> genIrStmts rest (IRSVarAssign(r, e)::IRSVarDecl(r, t)::acc)
  | SVarAssign(r, e)::rest -> genIrStmts rest ((IRSVarAssign(r, e))::acc)
  | _ -> print_endline "could not match at genIrStmts"; exit 1


let genIrBlock scope name =
  match scope with
  | SScope(stmts) -> IRBlock(name, (genIrStmts stmts []), (genIrBlockEnd stmts))

let genIrGlobal global = 
  match global with
  | GFuncDef(t, r, l, s) -> IRFunc(t, r, l, (genIrBlock s r))

let rec generate ast ir =
  match ast with
  | [] -> ir
  | (g::t) -> generate t ((genIrGlobal g)::ir)
  