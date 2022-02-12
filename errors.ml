
let lineNr = ref 0

let printErrorAndExit msg code =
  print_endline msg;
  exit code

let declarationError r =
  printErrorAndExit ("Variable \"" ^ r ^ "\" has already been declared and is being redeclared.") 2
let assignmentError r =
  printErrorAndExit ("Variable \"" ^ r ^ "\" has not been defined and is being assigned a value.") 2
let evaluationError r =
  printErrorAndExit ("Variable \"" ^ r ^ "\" is being defined with an expression that contains an undefined variable.") 2
let lexingError tok line = 
  printErrorAndExit ("Error on line: "^ line ^ "\n Token " ^ tok ^ " was not recognized.") 1