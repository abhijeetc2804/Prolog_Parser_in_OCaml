%{
type expr=  
  |VARIABLE of string 
  |ID of string
  |BOOL of string
  |FLOAT of string 
  |INT of string
  |PROGRAM of expr list
  |CLAUSE of expr
  |FACT of expr
  |RULE of expr*expr
  |HEAD of expr
  |BODY of expr list
  |ATOMIC_FORMULA of string*expr
  |FUNC of string*expr
  |TERM_LIST of expr list
  |COMMENT of expr
  |LIST of expr list
  |TUPLE of expr list
  |GOAL of expr list
%}

%token <string> INT
%token <string> ID 
%token <string> VARIABLE
%token <string> BOOL
%token <string> FLOAT
%token LIST TUPLE GOAL
%token LEFTP RIGHTP LEFT_SQ_BRACKET RIGHT_SQ_BRACKET 
%token COMMA COLON_DASH PERIOD SEMICOLON GOAL_HEAD
%token LEFT_COMMENT RIGHT_COMMENT 
%token EOF 

%start program           
%type <expr> program 
%%

program :
  | EOF { PROGRAM([]) }
  | clause program { 
    let PROGRAM ls_= $2 in 
    PROGRAM($1::ls_)
   }

clause:
  | rule PERIOD {
    CLAUSE ($1)
  }
  | fact PERIOD {
    CLAUSE ($1)
  }
  | GOAL_HEAD goal PERIOD{
    CLAUSE($2)
  }

// comment:
//   LEFT_COMMENT program RIGHT_COMMENT{
//     COMMENT($2)
//   }

// helper1:
//   |ID {
//       ID($1)
//   }

// helper2:
//   | VARIABLE {
//       VARIABLE($1)
//   }

fact :
  | head { FACT($1) }

rule :
  | head COLON_DASH body {
    RULE(HEAD($1),$3)
  }

body :
  | atomic COMMA body {
    let BODY lst =$3 in
    BODY([$1]@lst)
  }
  |atomic{
    BODY([$1])
  }
  | atomic SEMICOLON body {
    let BODY lst =$3 in
    BODY([$1]@lst)
  }
  
head :
  | atomic { $1 }

atomic :
  |ID LEFTP term_list RIGHTP { ATOMIC_FORMULA($1,$3)}

term :
  | LEFT_SQ_BRACKET possibly_empty_list RIGHT_SQ_BRACKET{ LIST([$2]) }
  | LEFTP possibly_empty_list RIGHTP { TUPLE([$2]) }
  | INT { INT($1) }
  | ID { ID($1) }
  | BOOL { BOOL($1)}
  | FLOAT { FLOAT($1) }
  | ID LEFTP term_list RIGHTP {FUNC($1 , $3)}
  | VARIABLE { VARIABLE ( $1 )}

possibly_empty_list:
  | /* empty */ {
      TERM_LIST([])
    }
  | term {
      TERM_LIST([$1])
    }
  | term COMMA term_list {
    let TERM_LIST lst = $3 in
      TERM_LIST($1::lst)
    }

term_list :
  | term {
      TERM_LIST([$1])
    }
  | term COMMA term_list {
    let TERM_LIST lst = $3 in
      TERM_LIST($1::lst)
    }

goal:
  | atomic {
      GOAL([$1])
    }
  | atomic COMMA goal {
    let GOAL ls_ =$3 in
    GOAL($1::ls_)
  }

// ocamllex lexer.mll
// ocamlc -c lexer.ml
// ocamlyacc parser.mly
// rm parser.mli
// ocamlc -c parser.ml
// ocamlc -o mycalc1.exe parser.ml lexer.ml main.ml
// ./mycalc1.exe
