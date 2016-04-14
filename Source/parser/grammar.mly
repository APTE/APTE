%{

open Parser_function

%}

%token <string> STRING
%token <int> INT


/* Token Special */
%token EQ NEQ
%token ANDECO
%token ORECO
%token AND
%token FUN SLASH FREE
%token LET IN OUT NEW
%token LPAR RPAR BARRE CHOICE PLUS
%token VIR PVIR DOT
%token IF THEN ELSE
%token CST ARGS LBRACE RBRACE TUPLE

%token EQUIVALENCE LENGTH SECRET PRESERVE
%token EOF
%token <int * int>PROJ

%left BARRE
%left PLUS
%nonassoc IN
%left PVIR
%left OR
%left AND


%start main /* the entry point */

%type <Parser_function.declaration> main
%%
/***********************************
***           Main Entry         ***
************************************/

main:
  | FUN ident SLASH INT DOT
      { FuncDecl ($2,$4) }
  | LET ident argument_list EQ process DOT
      { ProcDecl($2,$3,$5) }
  | LENGTH ident LBRACE length_definition RBRACE DOT
      { LengthDecl($2,$4) }
  | LENGTH TUPLE LPAR INT RPAR LBRACE length_definition RBRACE DOT
      { LengthTupleDecl($4,$7,(Parsing.symbol_start_pos ()).Lexing.pos_lnum) }
  | FREE ident DOT
      { FreeNameDecl($2) }
  | EQUIVALENCE process AND process DOT
      { Equivalence($2,$4) }
  | EQUIVALENCE LENGTH process AND process DOT
      { EquivalenceLength($3,$5) }
  | PRESERVE SECRET process DOT
      { Secrecy($3) }
  | EOF
      { raise End_of_file }
  | error
      { error_message (Parsing.symbol_start_pos ()).Lexing.pos_lnum "Syntax Error" }

/***********************************
***       Length definition      ***
************************************/

float_def:
  | INT DOT INT
      { let n = float_of_int $1 in
        let acc = ref (float_of_int $3) in
        while int_of_float !acc <> 0 do
          acc := !acc /. 10.
        done;
        n +. !acc
      }
  | INT DOT
      { float_of_int $1 }
  | INT
      { float_of_int $1 }


length_definition:
  | CST EQ float_def
      { ($3,[]) }
  | CST EQ float_def PVIR ARGS EQ length_arguments
      { ($3,$7) }

length_arguments:
  | float_def
      { [$1] }
  | float_def VIR length_arguments
      { $1::$3 }

/***********************************
***       Terms definition       ***
************************************/

ident :
  | STRING
      { ($1,(Parsing.symbol_start_pos ()).Lexing.pos_lnum) }

term:
  | ident
      { Id($1) }
  | PROJ LPAR term RPAR
      { let (i,n) = $1 in
        Proj(i,n,$3,(Parsing.symbol_start_pos ()).Lexing.pos_lnum) }
  | ident LPAR term_arguments RPAR
      { FuncApp($1,$3) }
  | LPAR term_arguments RPAR
      { if List.length $2 = 1
        then List.hd $2
        else Tuple($2) }

term_arguments :
  | term
      { [$1] }
  | term VIR term_arguments
      { $1::$3 }

argument_list:
  | variable_list
      { $1 }
  |
      { [] }

variable_list:
  | ident
      { [$1] }
  | ident variable_list
      { $1::$2 }

/***********************************
***      Pattern definition      ***
************************************/

pattern:
  | ident
      { PVar($1) }
  | LPAR pattern_arguments RPAR
      { PTuple($2) }

pattern_arguments :
  | pattern
      { [$1] }
  | pattern VIR pattern_arguments
      { $1::$3 }

/***********************************
***      Formula definition      ***
************************************/

formula:
  | term NEQ term
      { Neq($1,$3) }
  | term EQ term
      { Eq($1,$3) }
  | formula ANDECO formula
      { And($1,$3) }
  | formula ORECO formula
      { Or($1,$3) }

/***********************************
*** Definition du Process ***
************************************/

call_arguments:
  | term
      { [$1] }
  | term call_arguments
      { $1::$2 }

process:
  | INT
      { if $1 = 0 then Nil else error_message (Parsing.symbol_start_pos ()).Lexing.pos_lnum "Syntax Error" }
  | ident
      { Call($1,[]) }
  | ident call_arguments
      { Call($1,$2) }
  | LPAR process RPAR
      { $2 }
  | process BARRE process
      { Par($1,$3) }
  | process PLUS process
      { Choice($1,$3) }
  | SECRET term PVIR process
      { Secret($2,$4) }
  | NEW ident PVIR process
      { New($2,$4) }
  | IN LPAR term VIR ident RPAR PVIR process
      { In($3,$5,$8) }
  | OUT LPAR term VIR term RPAR PVIR process
      { Out($3,$5,$8) }
  | IN LPAR term VIR ident RPAR
      { In($3,$5,Nil) }
  | OUT LPAR term VIR term RPAR
      { Out($3,$5,Nil) }
  | IF formula THEN process ELSE process
      { IfThenElse($2,$4,$6) }
  | IF formula THEN process
      { IfThenElse($2,$4,Nil) }
  | LET pattern EQ term IN process
      { Let($2,$4,$6) }
