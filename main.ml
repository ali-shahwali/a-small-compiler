open Lexer
open Parser
open Pprint
open Analysis
open Gen_ir
open Ir_pprint
open Gen_asm_ir
open Asm_pprint
open Spill

let usage_msg = "append [--pretty-print] [--name-analysis] [--pretty-print-ir] [--asm] <file>"

let prettyPrint = ref false
let nameAnalysis = ref false
let prettyPrintIr = ref false
let asm = ref false

let input_files = ref []
let get_file filename = input_files := filename::!input_files
let speclist =
  [("--pretty-print", Arg.Set prettyPrint, "Pretty prints the AST");
  ("--name-analysis", Arg.Set nameAnalysis, "Performs name analysis");
  ("--pretty-print-ir", Arg.Set prettyPrintIr, "Pretty prints the intermediate representation");
  ("--asm", Arg.Set asm, "Pretty prints the assembly code");
  ]

let main =
  match Arg.parse_argv Sys.argv speclist get_file usage_msg with
  | exception Arg.Bad(s) -> print_endline s; exit 1
  | exception Arg.Help(s) -> print_endline s; exit 1
  | _ -> (
    match (List.length !input_files) == 1 with
    | true -> (
      let lexbuf = Lexing.from_channel (open_in (List.hd !input_files)) in
      if !prettyPrint then
        match Parser.main Lexer.token lexbuf with
        | ast -> Pprint.pprint ast
        | exception Parser.Error -> exit 1
      else if !nameAnalysis then
        match Parser.main Lexer.token lexbuf with
        | ast -> Analysis.analyze [] ast
        | exception Parser.Error -> exit 1
      else if !prettyPrintIr then
        match Parser.main Lexer.token lexbuf with
        | ast -> let ir = Gen_ir.generate ast [] in Ir_pprint.pprint ir
        | exception Parser.Error -> exit 1
      else if !asm then
        match Parser.main Lexer.token lexbuf with
        | ast -> 
          let IRFunc(_, _, _, block)::t = Gen_ir.generate ast [] in
          let block = Gen_asm_ir.inst_select_ir_block block in
          let block = Spill.spill block in 
          Asm_pprint.pprint block
        | exception Parser.Error -> exit 1
      else
        exit 1
    )
    | _ -> print_endline "Exactly one .c or .cpp file must be provided, run with --help for help"; exit 1
  )
  
  
