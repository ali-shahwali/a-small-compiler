{
    open Parser
    open Errors
}

let identRegxp = ['_' 'a'-'z' 'A'-'Z']['_' '0'-'9' 'a'-'z' 'A'-'Z']*
let hexRegxp = "0"['x' 'X']['0'-'9' 'a'-'f' 'A'-'F']+

rule token = parse
    | ['\n']
        { 
            Errors.lineNr := !Errors.lineNr + 1;
            token lexbuf 
        }
    | [' ' '\t']
        { token lexbuf }
    | hexRegxp as lxm
        { INT(int_of_string lxm) }
    | "//"[^'\n']*'\n'
        { 
            Errors.lineNr := !Errors.lineNr + 1; 
            token lexbuf 
        }
    | "/*" ([^'*'] | '*'+ [^'*''/'])* "*/" as lxm
        { 
            Errors.lineNr := !Errors.lineNr + (List.length (String.split_on_char '\n' lxm));
            token lexbuf 
        }
    | "#" [^'\n']*'\n'
        { Errors.lineNr := !lineNr + 1; token lexbuf }
    | '0'|['1'-'9']['0'-'9']* as lxm
        { INT(int_of_string lxm) }
    | "\'" [^'\'' '\\' '"' '\n' '\t'] "\'" as lxm
        { CHAR(String.get lxm 1) }
    | "\'" ['\\']['n'] "\'"
        { CHAR('\n') }
    | "\'" ['\\']['t'] "\'"
        { CHAR('\t') }
    | "\'" ['\\'] ['\\'] "\'"
        { CHAR('\\') }
    | "\'" ['\\'] ['\''] "\'"
        { CHAR('\'') }
    | "\'" ['\\'] ['\"'] "\'"
        { CHAR('\"') }
    | "\"" [^'\"']* "\"" as lxm
        { STRING(lxm) }
    | '+'
        { PLUS }
    | '-'
        { MINUS }
    | '*'
        { MUL }
    | '/'
        { DIV }
    | '%'
        { MOD }
    | '('
        { LPAREN }
    | ')'
        { RPAREN }
    | '{'
        { LBRACE }
    | '}'
        { RBRACE }
    | '['
        { LBRACKET }
    | ']'
        { RBRACKET }
    | '='
        { ASSIGN }
    | "++"
        { INCR }
    | "--"
        { DECR }
    | ','
        { DELIM }
    | "void"
        { TVOID }
    | "int"
        { TINT }
    | "char"
        { TCHAR }
    | "new"
        { NEW }
    | "break"
        { BREAK }
    | "while"
        { WHILE }
    | "for"
        { FOR }
    | "return"
        { RETURN }
    | "if"
        { IF }
    | "else"
        { ELSE }
    | "struct"
        { STRUCT }
    | "extern"
        { EXTERN }
    | '<'
        { LESS }
    | "<="
        { LESSEQ }
    | '>'
        { GREATER }
    | ">="
        { GREATEREQ }
    | "<<"
        { BSHIFTL }
    | ">>"
        { BSHIFTR }
    | "=="
        { EQ }
    | "!="
        { NEQ }
    | '&'
        { BWAND }
    | "&&"
        { AND }
    | '|'
        { BWOR }
    | "||"
        { OR }
    | '#'
        { MACRO }
    | ";"
        { NOP }
    | "."
        { DOT }
    | "!"
        { NOT }
    | "~" 
        { OCOMP }
    | identRegxp as lxm
        { IDENT(lxm) }
    | eof
        { EOF }
    | _ as lxm
        { Errors.lexingError (Char.escaped lxm) (string_of_int !lineNr)}