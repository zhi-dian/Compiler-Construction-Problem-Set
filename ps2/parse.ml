type token =
  | Plus
  | Minus
  | Times
  | Div
  | Eq
  | Neq
  | Lt
  | Lte
  | Gt
  | Gte
  | SemiColon
  | LPAREN
  | RPAREN
  | LLPAREN
  | LRPAREN
  | Equal
  | Not
  | And
  | Or
  | Assign
  | If
  | Else
  | For
  | While
  | Return
  | INT of (
# 37 "parse.mly"
        int
# 31 "parse.ml"
)
  | VAR of (
# 38 "parse.mly"
        string
# 36 "parse.ml"
)
  | EOF

open Parsing
let _ = parse_error;;
# 4 "parse.mly"
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
# 53 "parse.ml"
let yytransl_const = [|
  257 (* Plus *);
  258 (* Minus *);
  259 (* Times *);
  260 (* Div *);
  261 (* Eq *);
  262 (* Neq *);
  263 (* Lt *);
  264 (* Lte *);
  265 (* Gt *);
  266 (* Gte *);
  267 (* SemiColon *);
  268 (* LPAREN *);
  269 (* RPAREN *);
  270 (* LLPAREN *);
  271 (* LRPAREN *);
  272 (* Equal *);
  273 (* Not *);
  274 (* And *);
  275 (* Or *);
  276 (* Assign *);
  277 (* If *);
  278 (* Else *);
  279 (* For *);
  280 (* While *);
  281 (* Return *);
    0 (* EOF *);
    0|]

let yytransl_block = [|
  282 (* INT *);
  283 (* VAR *);
    0|]

let yylhs = "\255\255\
\001\000\002\000\002\000\005\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\006\000\006\000\
\006\000\006\000\006\000\006\000\006\000\006\000\003\000\003\000\
\003\000\003\000\003\000\003\000\003\000\004\000\004\000\000\000"

let yylen = "\002\000\
\002\000\001\000\003\000\001\000\003\000\001\000\001\000\003\000\
\002\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
\003\000\002\000\003\000\003\000\003\000\003\000\002\000\007\000\
\005\000\005\000\009\000\009\000\003\000\001\000\002\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\006\000\000\000\032\000\000\000\002\000\000\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\001\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\023\000\
\000\000\000\000\022\000\031\000\003\000\000\000\000\000\000\000\
\029\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\026\000\000\000\000\000\024\000\000\000\
\000\000\028\000\027\000"

let yydgoto = "\002\000\
\013\000\021\000\015\000\022\000\016\000\017\000"

let yysindex = "\007\000\
\079\255\000\000\044\255\044\255\079\255\044\255\000\255\006\255\
\018\255\044\255\000\000\016\255\000\000\043\000\000\000\125\255\
\000\000\007\255\028\000\041\255\079\255\045\255\028\000\044\255\
\044\255\044\255\144\255\044\255\000\000\044\255\044\255\044\255\
\044\255\044\255\044\255\044\255\044\255\044\255\044\255\000\000\
\044\255\044\255\000\000\000\000\000\000\163\255\182\255\201\255\
\000\000\028\000\007\255\007\255\039\255\039\255\001\255\001\255\
\001\255\001\255\001\255\001\255\028\000\028\000\079\255\044\255\
\079\255\037\255\220\255\000\000\079\255\044\255\000\000\239\255\
\063\255\000\000\000\000"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\106\255\000\000\000\000\000\000\000\000\
\000\000\092\000\000\000\047\000\048\255\000\000\003\255\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\004\255\105\000\118\000\066\000\079\000\042\255\127\000\
\136\000\145\000\154\000\163\000\010\255\011\255\000\000\000\000\
\000\000\001\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000"

let yygindex = "\000\000\
\000\000\255\255\000\000\045\000\003\000\065\000"

let yytablesize = 432
let yytable = "\014\000\
\025\000\030\000\031\000\032\000\033\000\018\000\019\000\001\000\
\023\000\032\000\033\000\024\000\027\000\018\000\021\000\018\000\
\021\000\025\000\041\000\042\000\019\000\020\000\019\000\020\000\
\041\000\042\000\046\000\047\000\048\000\026\000\050\000\028\000\
\051\000\052\000\053\000\054\000\055\000\056\000\057\000\058\000\
\059\000\060\000\029\000\061\000\062\000\003\000\012\000\012\000\
\012\000\012\000\012\000\012\000\012\000\043\000\012\000\004\000\
\041\000\042\000\069\000\045\000\006\000\066\000\030\000\068\000\
\003\000\044\000\067\000\071\000\020\000\011\000\012\000\075\000\
\072\000\074\000\004\000\000\000\005\000\000\000\000\000\006\000\
\003\000\000\000\000\000\007\000\000\000\008\000\009\000\010\000\
\011\000\012\000\004\000\000\000\005\000\000\000\000\000\006\000\
\000\000\000\000\000\000\007\000\000\000\008\000\009\000\010\000\
\011\000\012\000\007\000\007\000\007\000\007\000\007\000\007\000\
\007\000\007\000\007\000\007\000\007\000\000\000\007\000\000\000\
\000\000\000\000\000\000\007\000\007\000\030\000\031\000\032\000\
\033\000\034\000\035\000\036\000\037\000\038\000\039\000\040\000\
\000\000\000\000\000\000\000\000\000\000\000\000\041\000\042\000\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\049\000\000\000\000\000\000\000\000\000\000\000\
\000\000\041\000\042\000\030\000\031\000\032\000\033\000\034\000\
\035\000\036\000\037\000\038\000\039\000\000\000\000\000\063\000\
\000\000\000\000\000\000\000\000\041\000\042\000\030\000\031\000\
\032\000\033\000\034\000\035\000\036\000\037\000\038\000\039\000\
\064\000\000\000\000\000\000\000\000\000\000\000\000\000\041\000\
\042\000\030\000\031\000\032\000\033\000\034\000\035\000\036\000\
\037\000\038\000\039\000\000\000\000\000\065\000\000\000\000\000\
\000\000\000\000\041\000\042\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\070\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\042\000\030\000\
\031\000\032\000\033\000\034\000\035\000\036\000\037\000\038\000\
\039\000\000\000\000\000\073\000\000\000\000\000\000\000\000\000\
\041\000\042\000\025\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\025\000\000\000\025\000\025\000\
\000\000\025\000\000\000\000\000\000\000\025\000\000\000\025\000\
\025\000\025\000\025\000\025\000\030\000\031\000\032\000\033\000\
\034\000\035\000\036\000\037\000\038\000\039\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\041\000\042\000\004\000\
\004\000\004\000\004\000\004\000\004\000\004\000\004\000\004\000\
\004\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\004\000\004\000\010\000\010\000\010\000\010\000\010\000\010\000\
\010\000\010\000\010\000\010\000\010\000\000\000\010\000\011\000\
\011\000\011\000\011\000\011\000\011\000\011\000\011\000\011\000\
\011\000\011\000\000\000\011\000\009\000\009\000\000\000\000\000\
\009\000\009\000\009\000\009\000\009\000\009\000\009\000\000\000\
\009\000\005\000\005\000\000\000\000\000\005\000\005\000\005\000\
\005\000\005\000\005\000\005\000\000\000\005\000\008\000\008\000\
\000\000\000\000\008\000\008\000\008\000\008\000\008\000\008\000\
\008\000\000\000\008\000\013\000\013\000\013\000\013\000\013\000\
\013\000\013\000\000\000\013\000\014\000\014\000\014\000\014\000\
\014\000\014\000\014\000\000\000\014\000\015\000\015\000\015\000\
\015\000\015\000\015\000\015\000\000\000\015\000\016\000\016\000\
\016\000\016\000\016\000\016\000\016\000\000\000\016\000\017\000\
\017\000\017\000\017\000\017\000\017\000\017\000\000\000\017\000"

let yycheck = "\001\000\
\000\000\001\001\002\001\003\001\004\001\003\000\004\000\001\000\
\006\000\003\001\004\001\012\001\010\000\011\001\011\001\013\001\
\013\001\012\001\018\001\019\001\011\001\011\001\013\001\013\001\
\018\001\019\001\024\000\025\000\026\000\012\001\028\000\016\001\
\030\000\031\000\032\000\033\000\034\000\035\000\036\000\037\000\
\038\000\039\000\000\000\041\000\042\000\002\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\013\001\013\001\012\001\
\018\001\019\001\022\001\015\001\017\001\063\000\015\001\065\000\
\002\001\021\000\064\000\069\000\004\000\026\001\027\001\073\000\
\070\000\011\001\012\001\255\255\014\001\255\255\255\255\017\001\
\002\001\255\255\255\255\021\001\255\255\023\001\024\001\025\001\
\026\001\027\001\012\001\255\255\014\001\255\255\255\255\017\001\
\255\255\255\255\255\255\021\001\255\255\023\001\024\001\025\001\
\026\001\027\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\255\255\
\255\255\255\255\255\255\018\001\019\001\001\001\002\001\003\001\
\004\001\005\001\006\001\007\001\008\001\009\001\010\001\011\001\
\255\255\255\255\255\255\255\255\255\255\255\255\018\001\019\001\
\001\001\002\001\003\001\004\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\255\255\255\255\255\255\255\255\
\255\255\018\001\019\001\001\001\002\001\003\001\004\001\005\001\
\006\001\007\001\008\001\009\001\010\001\255\255\255\255\013\001\
\255\255\255\255\255\255\255\255\018\001\019\001\001\001\002\001\
\003\001\004\001\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\255\255\255\255\255\255\255\255\255\255\018\001\
\019\001\001\001\002\001\003\001\004\001\005\001\006\001\007\001\
\008\001\009\001\010\001\255\255\255\255\013\001\255\255\255\255\
\255\255\255\255\018\001\019\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\255\255\255\255\013\001\255\255\255\255\255\255\255\255\
\018\001\019\001\002\001\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\012\001\255\255\014\001\015\001\
\255\255\017\001\255\255\255\255\255\255\021\001\255\255\023\001\
\024\001\025\001\026\001\027\001\001\001\002\001\003\001\004\001\
\005\001\006\001\007\001\008\001\009\001\010\001\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\018\001\019\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\018\001\019\001\001\001\002\001\003\001\004\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\001\001\
\002\001\003\001\004\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\001\001\002\001\255\255\255\255\
\005\001\006\001\007\001\008\001\009\001\010\001\011\001\255\255\
\013\001\001\001\002\001\255\255\255\255\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\001\001\002\001\
\255\255\255\255\005\001\006\001\007\001\008\001\009\001\010\001\
\011\001\255\255\013\001\005\001\006\001\007\001\008\001\009\001\
\010\001\011\001\255\255\013\001\005\001\006\001\007\001\008\001\
\009\001\010\001\011\001\255\255\013\001\005\001\006\001\007\001\
\008\001\009\001\010\001\011\001\255\255\013\001\005\001\006\001\
\007\001\008\001\009\001\010\001\011\001\255\255\013\001\005\001\
\006\001\007\001\008\001\009\001\010\001\011\001\255\255\013\001"

let yynames_const = "\
  Plus\000\
  Minus\000\
  Times\000\
  Div\000\
  Eq\000\
  Neq\000\
  Lt\000\
  Lte\000\
  Gt\000\
  Gte\000\
  SemiColon\000\
  LPAREN\000\
  RPAREN\000\
  LLPAREN\000\
  LRPAREN\000\
  Equal\000\
  Not\000\
  And\000\
  Or\000\
  Assign\000\
  If\000\
  Else\000\
  For\000\
  While\000\
  Return\000\
  EOF\000\
  "

let yynames_block = "\
  INT\000\
  VAR\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    Obj.repr(
# 51 "parse.mly"
           ( _1 )
# 296 "parse.ml"
               : Ast.program))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rstmt) in
    Obj.repr(
# 54 "parse.mly"
          ((_1,rhs 1))
# 303 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'seq) in
    Obj.repr(
# 55 "parse.mly"
                        (_2)
# 310 "parse.ml"
               : Ast.stmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'rexp) in
    Obj.repr(
# 58 "parse.mly"
         ((_1,rhs 1))
# 317 "parse.ml"
               : 'exp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 61 "parse.mly"
               (Binop(_1,Plus,_3))
# 325 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 62 "parse.mly"
      (Int _1)
# 332 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 63 "parse.mly"
      (Var _1)
# 339 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 64 "parse.mly"
                (Binop(_1,Minus,_3))
# 347 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 65 "parse.mly"
            (Binop((Int 0,rhs 1),Minus,_2))
# 354 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 66 "parse.mly"
                (Binop(_1,Times,_3))
# 362 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 67 "parse.mly"
              (Binop(_1,Div,_3))
# 370 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 68 "parse.mly"
             (Binop(_1,Eq,_3))
# 378 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 69 "parse.mly"
              (Binop(_1,Neq,_3))
# 386 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 70 "parse.mly"
             (Binop(_1,Lt,_3))
# 394 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 71 "parse.mly"
              (Binop(_1,Lte,_3))
# 402 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 72 "parse.mly"
             (Binop(_1,Gt,_3))
# 410 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 73 "parse.mly"
              (Binop(_1,Gte,_3))
# 418 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 74 "parse.mly"
          (Not(_2))
# 425 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 75 "parse.mly"
              (And(_1,_3))
# 433 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 76 "parse.mly"
             (Or(_1,_3))
# 441 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : string) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'exp) in
    Obj.repr(
# 77 "parse.mly"
                (Assign(_1,_3))
# 449 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'rexp) in
    Obj.repr(
# 78 "parse.mly"
                     (_2)
# 456 "parse.ml"
               : 'rexp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 86 "parse.mly"
                (Exp(_1))
# 463 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : Ast.stmt) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 87 "parse.mly"
                                      (If(_3, _5, _7))
# 472 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 88 "parse.mly"
                            (If(_3, _5, (Exp(Int 0,0),rhs 1)))
# 480 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 89 "parse.mly"
                               (While(_3, _5))
# 488 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    let _9 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 90 "parse.mly"
                                                         (For(_3, _5, _7, _9))
# 498 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 6 : 'exp) in
    let _5 = (Parsing.peek_val __caml_parser_env 4 : 'exp) in
    let _7 = (Parsing.peek_val __caml_parser_env 2 : 'exp) in
    Obj.repr(
# 91 "parse.mly"
                                                              (For(_3, _5, _7, (Exp(Int 0,0),rhs 1)))
# 507 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'exp) in
    Obj.repr(
# 92 "parse.mly"
                      (Return(_2))
# 514 "parse.ml"
               : 'rstmt))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : Ast.stmt) in
    Obj.repr(
# 95 "parse.mly"
         (_1)
# 521 "parse.ml"
               : 'seq))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 1 : Ast.stmt) in
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'seq) in
    Obj.repr(
# 96 "parse.mly"
             ((Seq(_1,_2),rhs 1))
# 529 "parse.ml"
               : 'seq))
(* Entry program *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let program (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : Ast.program)