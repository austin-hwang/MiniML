/*
                         CS 51 Final Project
                           MiniML -- Parser
                             Spring 2018
*/
                  
%{
  open Printf ;;
  open Expr ;;
%}

%token EOF
%token OPEN CLOSE
%token LET DOT IN REC
%token NEG NOT NEGF
%token PLUS MINUS PLUSF MINUSF
%token TIMES DIVIDE TIMESF DIVIDEF
%token EXPONENT
%token AND OR
%token LESSTHAN EQUALS GREATERTHAN NOTEQUALS LESSEQUALS GREATEREQUALS
%token IF THEN ELSE 
%token FUNCTION
%token RAISE
%token <string> ID
%token <int> INT 
%token <float> FLOAT 
%token TRUE FALSE

%nonassoc LESSTHAN GREATERTHAN LESSEQUALS GREATEREQUALS
%nonassoc EQUALS NOTEQUALS
%nonassoc AND OR
%left PLUS MINUS PLUSF MINUSF
%left TIMES DIVIDE TIMESF DIVIDEF
%left NEG NEGF 
%left NOT

%start input
%type <Expr.expr> input

/* Grammar follows */
%%
input:  exp EOF                 { $1 }

exp:    exp expnoapp            { App($1, $2) }
        | expnoapp              { $1 }

expnoapp: INT                   { Num $1 }
        | FLOAT                 { Float $1 }
        | TRUE                  { Bool true }
        | FALSE                 { Bool false }
        | ID                    { Var $1 }
        | exp EXPONENT exp      { Binop(Exponent, $1, $3) }
        | exp PLUS exp          { Binop(Plus, $1, $3) }
        | exp MINUS exp         { Binop(Minus, $1, $3) }
        | exp TIMES exp         { Binop(Times, $1, $3) }
        | exp DIVIDE exp        { Binop(Divide, $1, $3) }
        | exp PLUSF exp         { Binop(Plusf, $1, $3) }
        | exp MINUSF exp        { Binop(Minusf, $1, $3) }
        | exp TIMESF exp        { Binop(Timesf, $1, $3) }
        | exp DIVIDEF exp       { Binop(Dividef, $1, $3) }
        | exp EQUALS exp        { Binop(Equals, $1, $3) }
        | exp NOTEQUALS exp     { Binop(NotEquals, $1, $3) }
        | exp LESSTHAN exp      { Binop(LessThan, $1, $3) }
        | exp GREATERTHAN exp   { Binop(GreaterThan, $1, $3) }
        | exp LESSEQUALS exp    { Binop(LessEquals, $1, $3) }
        | exp GREATEREQUALS exp { Binop(GreaterEquals, $1, $3) }
        | exp AND exp           { Binop(And, $1, $3) }
        | exp OR exp            { Binop(Or, $1, $3) }
        | NEG exp               { Unop(Negate, $2) }
        | NEGF exp              { Unop(Negatef, $2) }
        | NOT exp               { Unop(Not, $2) }
        | IF exp THEN exp ELSE exp      { Conditional($2, $4, $6) }
        | LET ID EQUALS exp IN exp      { Let($2, $4, $6) }
        | LET REC ID EQUALS exp IN exp  { Letrec($3, $5, $7) }
        | FUNCTION ID DOT exp   { Fun($2, $4) } 
        | RAISE                 { Raise }
        | OPEN exp CLOSE        { $2 }
;

%%
