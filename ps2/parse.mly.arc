/* Parser for Fish --- TODO */

%{
open Ast
open Lexing
(* use this to get the line number for the n'th token *)
let rhs n =
  let pos = Parsing.rhs_start_pos n in
  pos.pos_lnum
let parse_error s =
  let pos = Parsing.symbol_end_pos () in
  let l = pos.pos_lnum in
  print_string ("line "^(string_of_int l)^": "^s^"\n") 
%}

/* Tells us which non-terminal to start the grammar with. */
%start program

/* This specifies the non-terminals of the grammar and specifies the
 * types of the values they build. Don't forget to add any new non-
 * terminals here.
 */
%type <Ast.program> program
%type <Ast.stmt> stmt

/* The %token directive gives a definition of all of the terminals
 * (i.e., tokens) in the grammar. This will be used to generate the
 * tokens definition used by the lexer. So this is effectively the
 * interface between the lexer and the parser --- the lexer must
 * build values using this datatype constructor to pass to the parser.
 * You will need to augment this with your own tokens...
 */
%token Plus Minus Times Div Eq Neq Lt Lte Gt Gte SemiColon
%token LPAREN RPAREN LLPAREN LRPAREN Equal
%token Not And Or Assign
%token If Else For While Return
%token <int> INT 
%token <string> VAR
%token EOF

%nonassoc ELSE
%left Assign
%left Or
%left And
%left Not 
%left Eq Neq Lt Lte Gt Gte
%left Plus Minus
%left Times Div

/* Here's where the real grammar starts -- you'll need to add 
 * more rules here... Do not remove the 2%'s!! */
%%

program:
  stmt EOF { $1 }

exp :
exp Plus exp {(Binop($1,Plus,$3),rhs 1)}
| INT {(Int $1,rhs 1)}
| VAR {(Var $1,rhs 1)}
| exp Minus exp {(Binop($1,Minus,$3),rhs 1)}
| Minus exp {(Binop((Int 0,rhs 1),Minus,$2),rhs 1)}
| exp Times exp {(Binop($1,Times,$3),rhs 1)}
| exp Div exp {(Binop($1,Div,$3),rhs 1)}
| exp Eq exp {(Binop($1,Eq,$3),rhs 1)}
| exp Neq exp {(Binop($1,Neq,$3),rhs 1)}
| exp Lt exp {(Binop($1,Lt,$3),rhs 1)}
| exp Lte exp {(Binop($1,Lte,$3),rhs 1)}
| exp Gt exp {(Binop($1,Gt,$3),rhs 1)}
| exp Gte exp {(Binop($1,Gte,$3),rhs 1)}
| Not exp {(Not($2),rhs 1)}
| exp And exp {(And($1,$3),rhs 1)}
| exp Or exp {(Or($1,$3),rhs 1)}
| VAR Equal exp {(Assign($1,$3),rhs 1)}
| LPAREN exp RPAREN {$2}

stmt :
// /* empty */ {(Ast.skip, 0)}
| exp SemiColon {(Exp($1),rhs 1)}
| LLPAREN seq LRPAREN {$2}
| If LPAREN exp RPAREN stmt Else stmt {(If($3, $5, $7),rhs 1)}
| If LPAREN exp RPAREN stmt {(If($3, $5, (Exp(Int 0,0),rhs 1)),rhs 1)}
| While LPAREN exp RPAREN stmt {(While($3, $5),rhs 1)}
| For LPAREN exp SemiColon exp SemiColon exp RPAREN stmt {(For($3, $5, $7, $9),rhs 1)}
| For LPAREN exp SemiColon exp SemiColon exp RPAREN SemiColon {(For($3, $5, $7, (Exp(Int 0,0),rhs 1)),rhs 1)}
| Return exp SemiColon{(Return($2),rhs 1)}



seq :
   stmt {$1}
  | stmt seq {(Seq($1,$2),rhs 1)}