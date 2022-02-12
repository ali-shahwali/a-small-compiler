%token <int> INT
%token <char> CHAR
%token <string> IDENT
%token <string> STRING
%token BREAK "break"
%token WHILE "while"
%token FOR "for"
%token IF "if"
%token ELSE "else"
%token STRUCT "struct"
%token NEW "new"
%token RETURN "return"
%token TINT "int"
%token TCHAR "char"
%token TVOID "void"
%token EXTERN "extern"
%token NOP ";"
%token DELIM ","
%token DOT "."
%token INCR "++"
%token DECR "--"
%token LPAREN "("
%token RPAREN ")"
%token LBRACE "{"
%token RBRACE "}"
%token LBRACKET "["
%token RBRACKET "]"
%token NOT "!"
%token OCOMP "~"
%token PLUS "+"
%token MINUS "-"
%token MUL "*"
%token DIV "/"
%token MOD "%"
%token ASSIGN "="
%token BWAND "&"
%token AND "&&"
%token BWOR "|"
%token OR "||"
%token LESS "<"
%token GREATER ">"
%token LESSEQ "<="
%token GREATEREQ ">="
%token BSHIFTL "<<"
%token BSHIFTR ">>"
%token EQ "=="
%token NEQ "!="
%token OLCOMMENT "//"
%token MACRO "#"
%token SMLCOMMENT "/*"
%token EMLCOMMENT "*/"
%token EOF

%left OR
%left AND
%left BWOR
%left BWAND
%left EQ NEQ
%left LESS GREATER LESSEQ GREATEREQ
%left BSHIFTL BSHIFTR
%left PLUS MINUS
%left MUL DIV MOD

%right NOT OCOMP
%nonassoc UMINUS

%start <Ast.g list> main

%{
    open Ast
%}

%%


main: 
    | e = program { e }

%inline uop:
    | "!" { Not }
    | "~" { Comp }

%inline bop:
    | "*" { Mult }
    | "/" { Div }
    | "%" { Mod }
    | "+" { Plus }
    | "-" { Minus }
    | "<<" { BShiftL }
    | ">>" { BShiftR }
    | "<" { Less }
    | ">" { Greater }
    | "<=" { LessEq }
    | ">=" { GreaterEq }
    | "==" { Eq }
    | "!=" { Neq }
    | "&" { BWAnd }
    | "|" { BWOr }
    | "&&" { And }
    | "||" { Or }

ty:
    | "void" { TVoid }
    | "int" { TInt }
    | "char" { TChar }
    | r = IDENT { TIdent(r) }
    | t = ty "*" { TPoint(t) }

expr:
    | i = INT { EInt(i) }
    | c = CHAR { EChar(c) }
    | s = STRING { EString(s) }
    | v = IDENT { EVar(v) }
    | r = IDENT "(" e = separated_list(",", expr) ")"
        { ECall(r, e) }
    | "new" t = ty "[" e = expr "]" 
        {ENew(t, e)}
    | r = IDENT "[" e = expr "]" 
        { EArrayAccess(r, e, None) }
    | r1 = IDENT "[" e = expr "]" "." r2 = option(IDENT) 
        { EArrayAccess(r1, e, r2) }
    | e1 = expr b = bop e2 = expr 
        { EBinOp(b, e1, e2) }
    | MINUS e = expr %prec UMINUS
        { EUnOp(UMinus, e) }
    | u = uop e = expr
        { EUnOp(u, e) }
    | "(" e = expr ")" { e }

stmt:
    | s = varassign ";" { s }
    | "{" s = list(stmt) "}" 
        { SScope(s) }
    | "if" "(" e = expr ")" s1 = stmt "else" s2 = stmt
        { SIf(e, s1, Some(s2)) }
    | "if" "(" e = expr ")" s = stmt
        { SIf(e, s, None) }
    | "return" e = option(expr) ";" 
        { SReturn(e) }
    | "break" ";" 
        { SBreak }
    | "while" "(" e = expr ")" s = stmt 
        { SWhile(e, s) }
    | "for" "(" v = varassign ";" e = expr ";" a = assign ")" s = stmt
        { SScope([v;SWhile(e, SScope([s;a]))]) }
    
lvalue:
    | r = IDENT { r }
    

assign:
    | r = IDENT "(" e = separated_list(",", expr) ")"
        { SExpr(ECall(r, e)) }
    | r = IDENT "[" e1 = expr "]" "=" e2 = expr
        { SArrayAssign(r, e1, None, e2) }
    | r1 = IDENT "[" e1 = expr "]" "." r2 = option(IDENT) "=" e2 = expr
        { SArrayAssign(r1, e1, r2, e2) }
    | r = IDENT "[" e = expr "]" "++"
        { SArrayAssign(r, e, None, EBinOp(Plus, EArrayAccess(r, e, None), EInt(1))) }
    | r1 = IDENT "[" e = expr "]" "." r2 = option(IDENT) "++"
        { SArrayAssign(r1, e, None, EBinOp(Plus, EArrayAccess(r1, e, r2), EInt(1))) }
    | r = IDENT "[" e = expr "]" "--"
        { SArrayAssign(r, e, None, EBinOp(Minus, EArrayAccess(r, e, None), EInt(1))) }
    | r1 = IDENT "[" e = expr "]" "." r2 = option(IDENT) "--"
        { SArrayAssign(r1, e, None, EBinOp(Minus, EArrayAccess(r1, e, r2), EInt(1))) }
    | l = lvalue "=" e = expr 
        { SVarAssign(l, e) }
    | l = lvalue "++" 
        { SVarAssign(l, EBinOp(Plus, EVar(l), EInt(1))) }
    | l = lvalue "--"
        { SVarAssign(l, EBinOp(Minus, EVar(l), EInt(1))) }

varassign:
    | t = ty r = IDENT "=" e = expr 
        { SVarDef(t, r, e) }
    | a = assign { a }

params:
    p = separated_list(",", pair(ty, IDENT)) { p }

global:
    | t = ty r = IDENT "=" e = expr ";" 
        { GVarDef(t, r, e) }
    | "extern" t = ty r = IDENT ";" 
        { GVarDecl(t, r) }
    | "extern" t = ty r = IDENT "(" p = params ")" ";"
        { GFuncDecl(t, r, p) }
    | t = ty r = IDENT "(" p = params ")" "{" s = list(stmt) "}"
        { GFuncDef(t, r, p, SScope(s)) }
    | "struct" r = IDENT "{" l = separated_list(";", pair(ty, IDENT)) "}" ";" 
        { GStruct(r, l) }
    
program:
    | g = list(global) EOF { g }