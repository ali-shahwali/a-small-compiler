open Asm

let pprintReg n =
  match n with
  | 0 -> "rax"
  | 1 -> "rcx"
  | 2 -> "rdx"
  | 3 -> "rbx"
  | 4 -> "rsp"
  | 5 -> "rbp"
  | 6 -> "rsi"
  | 7 -> "rdi"
  | 8 -> "r8"
  | 9 -> "r9"
  | 10 -> "r10"
  | 11 -> "r11"
  | 12 -> "r12"
  | 13 -> "r13"
  | 14 -> "r14"
  | 15 -> "r15"
  | _ -> print_endline("No such register " ^ (string_of_int n) ^ " exists"); exit 1

let pprintUop uop =
  match uop with
  | Inc -> "inc"
  | Dec -> "dec"
  | Push -> "push"
  | Pop -> "pop"
  | IMul -> "imul"
  | IDiv -> "idiv"
  | Not -> "not"
  | Neg -> "neg"
  | Setg -> "setg"
  | Setl -> "setl"
  | Setge -> "setge"
  | Setle -> "setle"
  | Sete -> "sete"
  | Setne -> "setne"

let pprintOp op = 
  match op with
  | Imm(n) -> string_of_int n
  | Reg(r) -> pprintReg r
  | TReg(r, name) -> name ^ "_" ^ (string_of_int r)
  | Mem(r1, Some(r2), sc, disp) ->
    "[" ^ (pprintReg r1) ^ "+" ^ (string_of_int r2) ^ "*" ^ (string_of_int sc) ^ "+" ^ (string_of_int disp) ^ "]"
  | Mem(r1, None, sc, disp) -> 
    "qword [" ^ (pprintReg r1) ^ "*" ^ (string_of_int sc) ^ "+" ^ (string_of_int disp) ^ "]"
  | NoOp -> "nop"

let pprintBop bop =
  match bop with
  | Add -> "add\t"
  | Sub -> "sub\t"
  | Cmp -> "cmp\t"
  | Mov -> "mov\t"
  | And -> "and\t"
  | Or -> "or\t"
  | Xor -> "xor\t"

let rec pprintInstr instr =
  match instr with
  | BinOp(bop, op1, op2)::xs -> 
    "\t" ^ (pprintBop bop) ^ "\t" ^ (pprintOp op1) ^ ", " ^ (pprintOp op2) ^ "\n" ^ pprintInstr xs
  | UnOp(uop, op)::xs -> 
    "\t" ^ (pprintUop uop) ^ "\t" ^ (pprintOp op) ^ "\n" ^ pprintInstr xs
  | [] -> ""

let pprintEndInstr endInstr = 
  match endInstr with
  | Ret -> "\tret"

let pprintBlock block =
  match block with
  | Block(s, instr, endInstr) -> 
    print_endline (s ^ ":\n"); 
    print_endline (pprintInstr instr);  
    print_endline (pprintEndInstr endInstr);
    exit 0

let pprint block =
  print_endline "\tglobal\tmain";
  print_endline "\tsection\t.text";
  pprintBlock block