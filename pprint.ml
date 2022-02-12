open Ast

let removeSpecial c =
  match c with
  | "\n" -> "\\n"
  | "\t" -> "\\t"
  | "\\" -> "\\\\"
  | "\'" -> "\\'"
  | "\"" -> "\\\""
  | _ -> c

let pprintIdent ident = "\"" ^ ident ^ "\""

let pprintUop ast =
  match ast with
  | Not -> "!"
  | Comp -> "~"
  | UMinus -> "-"

let pprintBop ast =
  match ast with
  | Plus -> "+"
  | Minus -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Less -> "<"
  | Greater -> ">"
  | LessEq -> "<="
  | GreaterEq -> ">="
  | BShiftL -> "<<"
  | BShiftR -> ">>"
  | Eq -> "=="
  | Neq -> "!="
  | BWAnd -> "&"
  | BWOr -> "|"
  | And -> "&&"
  | Or -> "||"

let rec pprintType ast =
  match ast with
  | TIdent(r) -> "TIdent(" ^ r ^ ")"
  | TVoid -> "TVoid"
  | TInt -> "TInt"
  | TChar -> "TChar"
  | TPoint(t) -> "TPoint(" ^ (pprintType t) ^ ")"

let rec pprintExprList exprs =
  match exprs with
  | [] -> ""
  | (e::[]) -> pprintExpr e
  | (e::t) -> (pprintExpr e) ^ " " ^ pprintExprList t

and pprintExpr ast =
  match ast with
  | EInt(i) -> "EInt(" ^ (string_of_int i) ^ ")" 
  | EChar(c) -> "EChar(\'" ^ (removeSpecial(String.make 1  c)) ^ "\')"
  | EString(s) -> "EString(" ^ s ^ ")"
  | EVar(r) -> "EVar(\"" ^ r ^ "\")"
  | EUnOp(uop, e) -> "EUnOp(" ^ (pprintUop uop) ^ ", " ^ (pprintExpr e) ^ ")"
  | EBinOp(bop, e1, e2) -> "EBinOp(" ^ (pprintBop bop) ^ ", " ^ (pprintExpr e1) ^ ", " ^ (pprintExpr e2) ^ ")"
  | ECall(r, e) -> "ECall(" ^ (pprintIdent r) ^ ", {" ^ (pprintExprList e) ^ "})"
  | ENew(t, e) -> "ENew(" ^ (pprintType t) ^ ", " ^ (pprintExpr e) ^ ")"
  | EArrayAccess(r, e, None) -> "EArrayAccess(" ^ r ^ ", " ^ (pprintExpr e) ^ "," ^ ")"
  | EArrayAccess(r1, e, Some(r2)) -> "EArrayAccess(" ^ r1 ^ ", " ^ (pprintExpr e) ^ "," ^ r2 ^ ")"


let rec pprintScope stmts = 
  match stmts with
  | [] -> ""
  | (s::t) -> (pprintStmt s) ^ (pprintScope t)

and pprintStmt ast = 
  match ast with
  | SExpr(e) -> "SExpr(" ^ (pprintExpr e) ^ ")\n"
  | SVarDef(t, r, e) -> "SVarDef(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ", " ^(pprintExpr e) ^ ")\n"
  | SVarAssign(r, e) -> "SVarAssign(" ^ (pprintIdent r) ^ ", " ^ (pprintExpr e) ^ ")\n"
  | SScope(s) -> "SScope({\n" ^ (pprintScope s) ^ "})"
  | SWhile(e, s) -> "SWhile(" ^ (pprintExpr e) ^ ", " ^ (pprintStmt s) ^ ")\n"
  | SBreak -> "SBreak \n"
  | SReturn(Some(e)) -> "SReturn(" ^ (pprintExpr e) ^ ") \n"
  | SReturn(None) -> "SReturn() \n"
  | SIf(e, s, None) -> "SIf(" ^ (pprintExpr e) ^ ", \n" ^ (pprintStmt s) ^ ", ) \n"
  | SIf(e, s1, Some(s2)) -> "SIf(" ^ (pprintExpr e) ^ ", \n" ^ (pprintStmt s1) ^ ", " ^ (pprintStmt s2) ^ ")"
  | SArrayAssign(r, e1, None, e2) -> "SArrayAssign(" ^ r ^ ", " ^ (pprintExpr e1) ^ ", ," ^ (pprintExpr e2) ^ ")"
  | SArrayAssign(r1, e1, Some(r2), e2) -> "SArrayAssign(" ^ r1 ^ ", " ^ (pprintExpr e1) ^ ", " ^ r2 ^ ", " ^ (pprintExpr e2) ^ ")"
  | SDelete(r) -> "SDelete(" ^ r ^ ")"


let rec pprintParams params =
  match params with
  | [] -> ""
  | ((t, r)::[]) -> "(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ")" 
  | ((t, r)::p) -> "(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ") " ^ (pprintParams p) 

let pprintGlobal ast =
  match ast with
  | GVarDef(t, r, e) -> "GVarDef(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ", " ^ (pprintExpr e) ^ ")"
  | GFuncDef(t, r, l, s) -> "GFuncDef(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ", {" ^ (pprintParams l) ^ "}, \n" ^ (pprintStmt s) ^ ")"
  | GVarDecl(t, r) -> "GVarDecl(" ^ (pprintType t) ^ "," ^ (pprintIdent r) ^ ")"
  | GFuncDecl(t, r, l) -> "GFuncDecl(" ^ (pprintType t) ^ ", " ^ (pprintIdent r) ^ ", {" ^ (pprintParams l) ^ "})"
  | GStruct(r, l) -> "GStruct(" ^ r ^ ", " ^ "{" ^ (pprintParams l) ^ "}" ^ ")"
  | _ -> exit 1
  
let rec pprint ast =
  match ast with
  | [] -> exit 0
  | (h::t) -> print_endline (pprintGlobal h); pprint t