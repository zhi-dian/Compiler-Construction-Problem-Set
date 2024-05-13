(* This file defines a very simple interpreter for a little expression language
 * similar to Ocaml.  
*)

(*************************************************************************)
(* The module defines the abstract syntax or AST for the language *)
(*************************************************************************)
module Ast =
struct
  (* This is a type abbreviation *)
  type var = string 

  (* we only have one type for this little language *)
  type tipe = IntType

  (* Define the abstract syntax for a little expression language *)
  type binop = Plus | Minus | Times | Div

  type exp = 
      | Int of int
      | Binop of exp * binop * exp
      | Var of var
      | Let of var * exp * exp

  (* Some example expressions as abstract syntax trees *)

  (* let x = 42 in x * x *)
  let e1 = Let("x",Int 42,Binop(Var "x",Times,Var "x"))

  (* (let x = 42 in x * x) / 3 *)
  let e2 = Binop(e1,Div,Int 3)

  (* let y = ((let x = 42 in x * x) / 3) in y - 14 *)
  let e3 = Let("y",e2,Binop(Var "y",Minus,Int 14))

  (* Convert the abstract syntax tree to a string *)
  let rec exp2string(e:exp):string = 
    let binop2string(b:binop):string = 
      (match b with
          Plus -> "+"
        | Times -> "*"
        | Minus -> "-"
        | Div -> "/")
    in
      match e with
          Int i -> string_of_int i
        | Binop (e1,b,e2) -> 
            "(" ^ (exp2string e1) ^ " " ^ (binop2string b) ^ " " ^ (exp2string e2) ^ ")"
        | Var x -> x
        | Let(x,e1,e2) -> 
            "(let " ^ x ^ " = " ^ (exp2string e1) ^ " in " ^ (exp2string e2) ^ ")"
end (* module Ast *);;


(************************************************************************)
(* An abstract signature for environments                               *)
(************************************************************************)
module type ENV = 
sig
  type 'a env
  exception UnboundVariable of Ast.var
  val empty : unit -> 'a env
  val lookup : Ast.var -> 'a env -> 'a
  val extend : Ast.var -> 'a -> 'a env -> 'a env
end

(*************************************************************************)
(* One (inefficient) implementation of environments as association lists *)
(*************************************************************************)
module Env : ENV = 
struct
  open Ast
  type 'a env = (var * 'a) list

  (* Declares a new exception *)
  exception UnboundVariable of var

  (* The empty environment -- an empty association list *)
  let empty():'a env = []

  (* Lookup variable x in the environment, returning the associated value,
   * and raising the exception UnboundVariable if the variable is not found. *)
  let rec lookup(x:var) (env:'a env) = 
    match env with
        [] -> raise (UnboundVariable x)
      | (y,i)::rest -> if (x = y) then i else lookup x rest

  (* Extend env so that it maps x to i *)
  let extend (x:var) (i:'a) (env:'a env) = (x,i)::env
end

(**************************************************************************)
(* Evaluate an expression in an environment mapping variables to integers *)
(**************************************************************************)
module Eval : sig val evaluate : Ast.exp -> int end =
struct    
  open Ast
  let rec eval (e: exp) (env: int Env.env) : int = 
    let binop2fn b = 
      match b with
        | Plus -> (+)   (* op is needed for infix functions like + *)
        | Minus -> (-)
        | Times -> (fun x y -> x*y) (* conflicts with comments... *)
        | Div -> (/)
    in
      match e with
        | Int i -> i
        | Binop (e1,b,e2) -> (binop2fn b) (eval e1 env) (eval e2 env)
        | Var x -> Env.lookup x env
        | Let (x,e1,e2) -> 
            let i = eval e1 env in
              eval e2 (Env.extend x i env)

  (* Evaluate an expression -- start off with the empty environment *)
  let evaluate (e:exp) : int = eval e (Env.empty())
end

(* A little error support code *)
module Error =
struct
  exception Error of string
  let error s = raise (Error s)
end

(************************************************************************)
(* A simple type-checker for expressions                                *)
(************************************************************************)
module TypeCheck : sig val typecheck : Ast.exp -> Ast.tipe end =
struct
  open Ast
  (* Similar module to the evaluator...the only error we catch is unbound
   * variables. *)
  let rec tc (e : exp) (env : Ast.tipe Env.env) : tipe = 
    match e with
      | Int _ -> IntType
      | Binop (e1,_,e2) -> 
          (match (tc e1 env, tc e2 env) with
              (IntType,IntType) -> IntType)
      | Var x -> 
          (try Env.lookup x env with Env.UnboundVariable x -> 
            Error.error("unbound variable "^x))
      | Let (x,e1,e2) -> 
          let t = tc e1 env in
            tc e2 (Env.extend x t env)

  let typecheck (e : exp) : tipe = tc e (Env.empty())
end


(**************************************************************
***************************************************************
**
**                        Lexing and Parsing
**
***************************************************************
***************************************************************)

(*************************************************************)
(* This holds a couple of utilities for strings. *)
(*************************************************************)
module Util = 
struct
  (* explode a string into a list of characters *)
  let explode(s:string) : char list = 
    let rec loop i cs = 
      if i < 0 then cs else 
        loop (i - 1) ((String.get s i)::cs)
    in 
      loop (String.length s - 1) []

  (* collapse a list of characters into a string *)
  let implode(cs:char list) : string = 
    let buf = Bytes.create (List.length cs) in 
    let rec loop i cs = 
      match cs with 
        | [] -> buf
        | c::cs -> Bytes.set buf i c ; loop (i+1) cs
    in 
      Bytes.to_string (loop 0 cs)
end


(************************************************************************)
(* lexing -- break input into tokens                                    *)
(************************************************************************)
module type LEX = 
sig
  type token = 
        INT of int | VAR of Ast.var | PLUS | TIMES | MINUS | DIV | LET | IN |
        LPAREN | RPAREN | EQUALS | EOF
  val token2string : token -> string
  val lex : char list -> token * (char list)
  val lex_string : string -> token list
end

module Lex : LEX = 
struct
  type token = 
        INT of int | VAR of Ast.var | PLUS | TIMES | MINUS | DIV | LET | IN | 
        LPAREN | RPAREN | EQUALS | EOF

  let token2string t = 
    match t with
        INT i -> string_of_int i
      | VAR x -> x
      | PLUS -> "+"
      | MINUS -> "-"
      | DIV -> "/"
      | TIMES -> "*"
      | LET -> "let"
      | IN -> "in"
      | LPAREN -> "("
      | RPAREN -> ")"
      | EQUALS -> "="
      | EOF -> "<end-of-file>"

  let rec lex (cs: char list) : token * (char list) = 
    (match cs with
      | [] -> (EOF,[])
      | ' ' :: rest -> lex rest   (* skip whitespace *)
      | '\n' :: rest -> lex rest  (* skip whitespace *)
      | '(' :: '*' :: rest -> lex_comment 1 rest (* comment start *)
      | '+' :: rest -> (PLUS,rest)
      | '-' :: rest -> (MINUS,rest)
      | '*' :: rest -> (TIMES,rest)
      | '/' :: rest -> (DIV,rest)
      | '(' :: rest -> (LPAREN,rest)
      | ')' :: rest -> (RPAREN,rest)
      | '=' :: rest -> (EQUALS,rest)
      | c :: rest -> 
          if (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') then 
            let (x,rest) = lex_var [c] rest in
              if x = "let" then (LET,rest)      (* check keywords *)
              else if x = "in" then (IN, rest)
              else (VAR x, rest)
          else if (c >= '0' && c <= '9') then 
            lex_num ((int_of_char c) - (int_of_char '0')) rest
          else 
            Error.error (Printf.sprintf "bad character %c" c))

  (* process comments -- depth records the nesting depth of comments -- we go back
   * to the lexer only when it drops down to zero *)
  and lex_comment (depth:int) (cs:char list) : token * (char list) = 
    (match cs with
      | '*' :: ')' :: rest -> (* close comment*)
          let new_depth = depth - 1 in (* see if nesting depth drops to zero *)
            if new_depth = 0 then lex rest  (* if so, continue with lexer *)
            else lex_comment new_depth rest (* otherwise, continue with comment *)
      | '(' :: '*' :: rest -> lex_comment (depth + 1) rest  (* increase comment depth *)
      | c :: rest -> lex_comment depth rest  (* skip any other characters *)
      | [] -> Error.error "missing comment end") (* oops -- missing end comment *)

  (* accum represents the value of the number so far *)
  and lex_num (accum:int) (cs:char list) : token * (char list) = 
    (match cs with
      | c :: rest -> 
          (* see if c is a digit *)
          if c >= '0' && c <= '9' then 
            (* convert the digit to an integer and fold into accumulator *)
            lex_num (accum*10 + (int_of_char c) - (int_of_char '0')) rest
          else (INT accum, cs)
      | [] -> (INT accum, cs))

  (* accum represents the identifier so far, but as a reversed list of characters *)
  and lex_var (accum:char list) (cs:char list) : string * (char list) = 
    (match cs with
      | c :: rest ->
          (* make sure c is a letter, digit, or underscore -- if so, push it on accum *)
          if c = '_' || (c >= 'a' && c <= 'z') || (c >= 'A' && c >= 'Z') then 
            lex_var (c::accum) rest
            (* be sure to reverse the list and then collapse it with implode *)
          else (Util.implode(List.rev accum), cs)
      | [] -> (Util.implode(List.rev accum), cs))

  let rec lex_all (cs: char list) =
    match lex cs with
    | (EOF,[]) -> [EOF]
    | (EOF,_) -> Error.error("junk after EOF")
    | (t, cs') -> t :: lex_all cs'

  let lex_string (s: string) =
    lex_all (Util.explode s)

end (* Lex *)

(************************************************************************)
(* parsing -- take in a string, use the lexer to tokenize it, and       *)
(* build an expression.                                                 *)
(************************************************************************)
module Parse : sig val parse : string -> Ast.exp end = 
struct
  open Ast
  open Lex

  let token_error (s:string) (t:token) = 
    Error.error("expecting "^s^" but found '"^(token2string t)^"'")

  (* aexp ::= INT | VAR | '(' exp ')' | 'let' VAR '=' exp 'in' exp *)
  let rec parse_aexp (cs : char list) : exp * (char list) = 
    (match lex cs with
      | (INT i, cs) -> (Int i, cs)
      | (VAR x, cs) -> (Var x, cs)
      | (LPAREN, cs) -> 
          (let (e,cs) = parse_exp cs in
             match lex cs with
               | (RPAREN, cs) -> (e, cs)
               | (t,_) -> token_error "')'" t)
      | (LET, cs) -> 
          (match lex cs with
            | (VAR x, cs) -> 
                (match lex cs with
                  | (EQUALS,cs) -> 
                      (match parse_exp cs with
                        | (e1,cs) -> 
                            (match lex cs with
                              | (IN,cs) -> 
                                  (match parse_exp cs with
                                    | (e2,cs) -> (Let(x,e1,e2),cs))
                              | (t,_) -> token_error "'in'" t))
                  | (t,_) -> token_error "'='" t)
            | (t,_) -> token_error "<variable>" t)
      | (t,_) -> token_error "<int> or <variable> or '(' or 'let'" t)

  (* term ::= aexp | term '*' aexp | term '/' aexp *)
  and parse_term (cs:char list) : exp * (char list) = 
    (let rec loop(e,cs) = 
      match lex cs with
          (TIMES,cs) -> 
            let (e2,cs) = parse_aexp cs in loop(Binop(e,Times,e2),cs) 
        | (DIV,cs) ->
            let (e2,cs) = parse_aexp cs in loop(Binop(e,Div,e2),cs) 
        | _ -> (e,cs)
     in
       loop (parse_aexp cs))

  (* exp ::= term | exp '+' term | exp '-' term *)
  and parse_exp (cs:char list) : exp * (char list) = 
    (let rec loop(e,cs) = 
      match lex cs with
          (PLUS,cs) -> 
            let (e2,cs) = parse_term cs in loop(Binop(e,Plus,e2),cs) 
        | (MINUS,cs) ->
            let (e2,cs) = parse_term cs in loop(Binop(e,Minus,e2),cs)
        | _ -> (e,cs)
     in
       loop (parse_term cs))

  (* prog ::= exp EOF *)
  let parse (s:string) : exp = 
    let (e,cs) = parse_exp (Util.explode s) in
      match lex cs with
          (EOF,_) -> e
        | (t,_) -> token_error "<end-of-file>" t
end (* parse *)

(************************************************************************)
(* Put it all together                                                  *)
(************************************************************************)
module MyLanguage : sig val calc : string -> unit end = 
struct
  let calc(x:string) = 
    let e = Parse.parse x in
    let _ = TypeCheck.typecheck e in
    let i = Eval.evaluate e in
      try 
        Printf.printf "The result is %d\n" i
      with 
        | Error.Error s -> Printf.printf "error: %s\n" s
end

