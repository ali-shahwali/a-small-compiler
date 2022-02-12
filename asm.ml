type unop = 
  | Inc 
  | Dec 
  | Push 
  | Pop 
  | IMul 
  | IDiv 
  | Not 
  | Neg 
  | Setg 
  | Setl
  | Setge 
  | Setle 
  | Sete 
  | Setne

type binop = 
  | Add 
  | Sub 
  | Cmp 
  | Mov 
  | And 
  | Or 
  | Xor

type jbinop =
  | Jl 
  | Jg 
  | Jle 
  | Jge 
  | Je 
  | Jne

type reg = int

type displacement = int

type scale = int

type op =
  | Imm of int 
  | Reg of reg
  | TReg of reg * string 
  | Mem of reg * reg option * scale * displacement
  | NoOp


type inst =
  | UnOp  of unop * op
  | BinOp of binop * op * op
  | Call  of string
  | Cqo

type blockend =
  | Ret
  | Jmp of string
  | JBinOp of jbinop * string * string

type block = Block of string * inst list * blockend

type func = Func of string * block