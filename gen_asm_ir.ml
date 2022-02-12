open Asm
open Ir
open Ast

let tmp_reg n = TReg(n, "tmp")

let make_reg l env =
  let (n, ty) = List.assoc l env in
  TReg(n, l)

let rec inst_select_expr env n acc reg expr =
  match expr with
  | EVar(x) -> (Asm.BinOp(Asm.Mov, reg, (make_reg x env))::acc, n)
  | EInt(v) -> (Asm.BinOp(Asm.Mov, reg, Imm(v))::acc, n)
  | EBinOp(Plus, EVar(x), EInt(v)) | EBinOp(Plus, EInt(v), EVar(x))
      when reg = make_reg x env -> (Asm.BinOp(Asm.Add, reg, Imm(v))::acc, n)
  | EBinOp(Plus, EVar(x), EVar(y))   
      when reg = make_reg x env -> (Asm.BinOp(Asm.Add, reg, make_reg y env)::acc, n)
  | EBinOp(Plus, EVar(y), EVar(x))   
      when reg = make_reg x env -> (Asm.BinOp(Asm.Add, reg, make_reg y env)::acc, n)
  | EBinOp(Minus, EVar(x), EInt(v)) | EBinOp(Minus, EInt(v), EVar(x))
      when reg = make_reg x env -> (Asm.BinOp(Asm.Sub, reg, Imm(v))::acc, n)
  | EBinOp(Minus, EVar(x), EVar(y))   
      when reg = make_reg x env -> (Asm.BinOp(Asm.Sub, reg, make_reg y env)::acc, n)
  | EBinOp(Minus, EVar(y), EVar(x))   
      when reg = make_reg x env -> (Asm.BinOp(Asm.Sub, reg, make_reg y env)::acc, n)
  | EBinOp(Plus, e1, e2) ->
      let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
      let n1 = n + 2 in
      let acc1 = Asm.BinOp(Asm.Mov, reg, r1)::Asm.BinOp(Asm.Add, reg, r2)::acc in
      let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
      inst_select_expr env n2 acc2 r1 e1
  | EBinOp(Minus, e1, e2) ->
      let (r1, r2) = (tmp_reg n, tmp_reg (n+1)) in
      let n1 = n + 2 in
      let acc1 = Asm.BinOp(Asm.Mov, reg, r1)::Asm.BinOp(Asm.Sub, reg, r2)::acc in
      let (acc2, n2) = inst_select_expr env n1 acc1 r2 e2 in
      inst_select_expr env n2 acc2 r1 e1
  | _ -> print_endline "couldnt match with ir expr"; exit 1


let rec inst_select_ir_stmts env n acc stmts = 
  match stmts with
  | IRSVarAssign(x, expr)::xs ->
    let (expr_acc, n2) = inst_select_expr env n [] (make_reg x env) expr in
    inst_select_ir_stmts env n2 (List.rev_append expr_acc acc) xs
  | IRSVarDecl(x, ty)::xs ->
    inst_select_ir_stmts ((x, (n,ty))::env) (n+1) acc xs
  | [] -> (env, n, List.rev acc)
  | _ -> print_endline "couldnt match with ir stmt"; exit 1

let inst_select_ir_block_end env n acc blockEnd =
  match blockEnd with
  | IRSReturn(Some(expr)) -> 
    let (expr_acc, n2) = inst_select_expr env n [] (Reg(0)) expr in
    (Some(expr_acc), Asm.Ret)
  | IRSReturn(None) -> (None, Asm.Ret)
  | _ -> print_endline "couldnt match with block end"; exit 1

let inst_select_ir_block block =
  match block with
  | IRBlock(s, stmts, blockEnd) -> (
    let (env, n, acc) = inst_select_ir_stmts [] 0 [] stmts in
    match inst_select_ir_block_end env n [] blockEnd with
    | (None, ret) -> Asm.Block(s, acc, ret)
    | (Some(stmt), ret) -> Asm.Block(s, List.append acc stmt, ret)
  )
  | _ -> print_endline "could not match with ir block"; exit 1
    
