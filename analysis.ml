open Ast
open Errors


let rec isDefined r env =
  match env with
  | [] -> false
  | (i::t) -> (if i = r then true else isDefined r t)

let rec add idents env =
  match idents with
  | [] -> env
  | ((_, r)::t) -> (
    if isDefined r env then 
      Errors.declarationError r
    else(
      add t (r::env))
    )
  
let createScope stmt =
  match stmt with
  | SScope(s) -> s
  | _ -> stmt::[]


let rec validExpr env expr  =
  match expr with
  | EVar(r) -> (if isDefined r env then true else false)
  | EBinOp(_, e1, e2) -> (if validExpr env e1 && validExpr env e2 then true else false)
  | EUnOp(_, e) -> (if validExpr env e then true else false)
  | ECall(r, e) -> (if (isDefined r env) && (List.for_all (validExpr env) e) then true else false)
  | _ -> true

let rec analyzeScope oldEnv env scope =
  match scope with
  | [] -> oldEnv
  | (SVarDef(ty, r, e)::t) -> 
    (if validExpr env e then 
      analyzeScope oldEnv (add ((ty, r)::[]) env) t 
    else 
      Errors.evaluationError r)
  | (SVarAssign(r, e)::t) -> 
    (if isDefined r env && validExpr env e then 
      analyzeScope oldEnv env t
     else 
      Errors.assignmentError r)
  | (SScope(s)::t) -> analyzeScope oldEnv (analyzeScope env env s) t
  | (SWhile(e, s)::t) -> 
    (if validExpr env e then
      analyzeScope oldEnv (analyzeScope env env (createScope s)) t
    else
      Errors.evaluationError "inside while loop"
      )
  | (SIf(e, s, None)::t) ->  
    (if validExpr env e then 
      analyzeScope oldEnv (analyzeScope env env  (createScope s)) t
    else
      Errors.evaluationError "inside if statement")
  | (SIf(e, s1, Some(s2))::t) -> 
    (if validExpr env e then 
      analyzeScope oldEnv (analyzeScope env (analyzeScope env env (createScope s1)) (createScope s2)) t
    else
      Errors.evaluationError "inside if statement")
  | (SReturn(Some(e))::t) -> 
    (if validExpr env e then 
      analyzeScope oldEnv env t 
    else 
      Errors.evaluationError "inside return statement")
  | (_::t) -> analyzeScope oldEnv env t
  
let rec analyzeGlobal env ast =
  match ast with
  | GVarDef(t, r, e) -> 
    if validExpr env e then 
      add ((t,r)::[]) env 
    else evaluationError r
  | GFuncDef(t, r, l, SScope(s)) -> 
    let oldEnv = add ((t,r)::[]) env in
    let newEnv = add l env in
    analyzeScope oldEnv newEnv  s
  | GVarDecl(t, r) -> add ((t,r)::[]) env
  | GFuncDecl(t, r, l) -> add ((t,r)::[]) env
  | _ -> exit 1

let rec analyze env ast =
  match ast with
  | [] -> print_endline "There were no name errors"; exit 0
  | (g::t) -> analyze (analyzeGlobal env g) t 