open Asm

let stack = ref (-8)


let spillReg reg env memloc = 
  try ((reg, (List.assoc reg env)), env) with
  Not_found -> 
    stack := !stack + 8;
    ((reg, !stack), ((reg, !stack)::env)) 

let rec spillInstr instr memloc env acc =
  match instr with
  | BinOp(bop, Reg(n), Imm(n2))::xs ->
    spillInstr xs memloc env (BinOp(bop, Reg(n), Imm(n2))::acc)
  | BinOp(bop, reg, Imm(n))::xs -> 
      let ((r, loc), newEnv) = spillReg reg env memloc in
      spillInstr xs loc newEnv (BinOp(bop, Mem(4, None, 1, loc), Imm(n))::acc)
  | BinOp(bop, Reg(n), reg)::xs -> 
      let ((r, loc), newEnv) = spillReg reg env memloc in
      spillInstr xs loc newEnv (BinOp(bop, Reg(n), Mem(4, None, 1, loc))::acc)
  | BinOp(bop, reg1, reg2)::xs -> 
      let ((r, loc), newEnv) = spillReg reg1 env memloc in
      let ((r2, loc2), newEnv) = spillReg reg2 newEnv loc in
      let acc = (BinOp(Mov, Reg(10), Mem(4, None, 1, loc2))::acc) in
      spillInstr xs loc2 newEnv (BinOp(bop, Mem(4, None, 1, loc), Reg(10))::acc)
  | [] -> List.rev acc
     
  (*  *)

let spill block =
  match block with
  | Block(s, instr, blockEnd) -> 
    let spilled = spillInstr instr (-8) [] [] in
    let spilled = (BinOp(Sub, Reg(4), Imm(!stack + 8))::spilled) in
    let spilled = List.append spilled [BinOp(Add, Reg(4), Imm(!stack + 8))] in
    Block(s, spilled, blockEnd)