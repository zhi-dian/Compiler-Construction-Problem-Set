(**************************************)
(* Some utilies                       *)
(**************************************)

(* explode a string into a list of characters *)
let explode(s:string) : char list = 
  let rec loop i cs = 
    if i < 0 then cs else 
      loop (i - 1) ((String.get s i)::cs)
  in 
    loop (String.length s - 1) []

(* collapse a list of characters into a string *)
let implode(cs:char list) : string = 
  let buf = String.create (List.length cs) in 
  let rec loop i cs = 
    match cs with 
      | [] -> buf
      | c::cs -> String.set buf i c ; loop (i+1) cs
  in 
    loop 0 cs

(* just a wrapper for consing a pair of values *)
let cons(x,y) = x::y 

(* Generalizes in two ways over our earlier regexps:  First,
   we work over an arbitrary token type (not just characters).
   Second, we see that the combinators seem to work for at
   least some classes of recursive grammars -- notably, those
   without left recursion. *)
module Parse = 
struct

  (* Here the type variable 'tok represents the input type 
     to the grammar.  For instance, a lexer will have char
     as the input type, whereas a parser will have token
     as the input type. *)
  type ('tok,'a) grammar = 'tok list -> ('a * ('tok list)) list 

  (* Given a grammar and list of tokens of the appropriate
     input type, runs the grammar, and filters out all of the
     results that don't use all of the tokens. *)
  let parse (r:('tok,'a) grammar) (tokens : 'tok list) : 'a list = 
    let results = r tokens in
    let uses_all = List.filter (fun p -> snd p = []) results in 
      List.map fst uses_all 

  (* The following definitions are exactly the same as our
     development with regexps, except they are generalized
     for the input type. *)
  let ch(c:char) : (char,char) grammar = 
    function 
      | c'::rest -> if c = c' then [(c,rest)] else []
      | _ -> []
  
  let eps : ('tok,unit) grammar = fun (s:'tok list) -> [((), s)] 

  let void : ('tok,'a) grammar = fun s -> [] 

  let (%) (r:('tok,'a) grammar) (f:'a -> 'b) : ('tok,'b) grammar = 
    fun s -> 
      List.map (function (v,s') -> (f v,s')) (r s) 

  let (++)(r1:('tok,'a) grammar) (r2:('tok,'a) grammar) : 
      ('tok,'a) grammar = 
    fun s -> (r1 s) @ (r2 s)

  (* This is similar to "++" except that it prefers to match
     the first grammar and only if that fails does it try to
     match against the second grammar.  So it's a left-biased
     alternative. *)
  let (>>)(r1:('tok,'a) grammar) (r2:('tok,'a) grammar) : 
     ('tok,'a) grammar = 
      fun ts -> 
        match r1 ts with 
        | [] -> r2 ts
        | s -> s

  let ($)(r1: ('tok,'a) grammar) (r2: ('tok,'b) grammar) : 
      ('tok, 'a * 'b) grammar = 
  fun s -> 
    List.fold_right 
      (function (v1,s1) -> fun res -> 
         (List.fold_right 
            (function (v2,s2) -> 
               fun res -> ((v1,v2),s2)::res) (r2 s1) res)) (r1 s) [] 

  let rec star(r:('tok,'a) grammar) : ('tok, 'a list) grammar = 
    fun s -> (((r $ (star r)) % cons) ++ (eps % (fun _ -> []))) s 

  (* This is similar to start but uses the left-biased alternative
     to prefer a longer match over a shorter one. *)
  let rec greedy_star (r:('tok,'a) grammar) : ('tok, 'a list) grammar =
    fun s -> (((r $ (greedy_star r)) % cons) >> (eps % (fun _ -> []))) s

  let plus(r: ('tok,'a) grammar) : ('tok,'a list) grammar = 
    (r $ (star r)) % cons 

  (* And greedy plus also goes for a longest match. *)
  let greedy_plus(r: ('tok,'a) grammar) : ('tok,'a list) grammar = 
    (r $ (greedy_star r)) % cons

  let (%%) (r:('tok,'a) grammar) (v:'b) : ('tok,'b) grammar = 
    r % (fun _ -> v) 
  
  let opt(r:('tok,'a) grammar) : ('tok,'a option) grammar = 
    (r % (fun x -> Some x)) ++ (eps %% None);;
  
  let alts (rs: ('tok,'a) grammar list) : ('tok,'a) grammar = 
    List.fold_right (++) rs void 
  
  let cats (rs: ('tok,'a) grammar list) : ('tok,'a list) grammar = 
    List.fold_right (fun r1 r2 -> (r1 $ r2) % cons) rs 
      (eps % (fun _ -> [])) 
  
  let digit : (char,char) grammar = 
    alts (List.map (fun i -> ch (char_of_int (i + (int_of_char '0'))))
            [0;1;2;3;4;5;6;7;8;9])
  
  (* We now define naturals using greedy plus to get the longest
     integer we can out of it.  This avoids ambiguity later on in
     our definition of tokens. *)
  let natural : (char,int) grammar = 
    (greedy_plus digit) %
      (List.fold_left 
         (fun a c -> a*10 + (int_of_char c) - (int_of_char '0')) 0)
      
  let integer : (char,int) grammar = 
    natural ++ (((ch '-') $ natural) % (fun (_,n) -> -n)) 
  
  let rec gen(i:int)(stop:int) : int list = 
    if i > stop then [] else i::(gen (i+1) stop)
  
  let lc_alpha : (char,char) grammar =
    let chars = List.map char_of_int (gen (int_of_char 'a') (int_of_char 'z')) in
      alts (List.map ch chars)
  
  let uc_alpha : (char,char) grammar = 
    let chars = List.map char_of_int (gen (int_of_char 'A') (int_of_char 'Z')) in
      alts (List.map ch chars)

  (* Similar to naturals, we want the longest parse for an identifier, so
     we use the greedy star here. *)
  let identifier : (char,string) grammar = 
    (lc_alpha $ (greedy_star (alts [lc_alpha; uc_alpha; ch '_'; digit]))) %
      (fun (c,s) -> implode (c::s))
  
  type token = 
      INT of int | ID of string | LET | IN | PLUS | TIMES | MINUS | DIV | LPAREN
    | RPAREN | EQ ;;

  let keywords = [ ("let",LET) ; ("in",IN) ]
  
  (* here are the token grammars for our little ML language *)
  let token_grammars = [
    integer % (fun i -> INT i) ; 
    identifier % (fun s -> 
                    try List.assoc s keywords
                    with Not_found -> ID s) ; 
    (ch '+') %% PLUS ; 
    (ch '*') %% TIMES ; 
    (ch '-') %% MINUS ; 
    (ch '/') %% DIV ; 
    (ch '(') %% LPAREN ; 
    (ch ')') %% RPAREN ;
    (ch '=') %% EQ ; 
  ];;

  (* so we can define a grammar to match any legal token *)
  let token = alts token_grammars ;;
  
  (* white space *)
  let ws = alts [ch ' ' ; ch '\n' ; ch '\r' ; ch '\t']

  (* one or more white space *)
  let wsp = plus ws
  (* zero or more white spaces *)
  let wsz = star ws

  (* document -- zero or more tokens separated by white spaces.  Now that
     we use greedy numbers and identifiers, we don't need to have white
     space between our tokens. *)
  let doc : (char,token list) grammar = 
    ((wsz $ (star ((token $ wsz) % fst))) % snd)

  (* Define lex in terms of our parse function above. *)
  let lex(s:string) : token list = 
    match parse doc (explode s) with
      | ts::[] -> ts
      | [] -> raise (Failure "lexing failure")
      | _ -> raise (Failure "ambiguous")


  (* Here, we define a parser taking us from tokens to 
     the abstract syntax for a little fragment of OCaml. *)

  type var = string
  type binop = Plus | Minus | Times | Div
  type exp = 
    | Int of int
    | Binop of exp * binop * exp
    | Var of var
    | Let of var * exp * exp

  (* Sort of a generalization of the "ch" constructor.  Here,
     we accept any input token for which the function pred
     returns SOMEthing.  The output is the SOMEthing returned.
     If pred returns None on that input token, or there's
     no token, we reject. *)
  let satisfies (pred:'a -> 'b option) : ('a,'b) grammar = 
    fun ts -> 
      match ts with 
      | [] -> []
      | t::ts -> (match pred t with 
                    | None -> []
                    | Some v -> [(v,ts)])

  (* Accepts any (INT i) token and returns i *)
  let int_p : (token,exp) grammar = 
    satisfies (function (INT i) -> Some i | _ -> None) % (fun i -> Int i)

  (* Accepts any (ID x) token. *)
  let var_p : (token,string) grammar = 
    satisfies (function (ID x) -> Some x | _ -> None) 

  (* Accepts any token equal to t and returns unit. *)
  let tok (t:token) : (token,unit) grammar = 
    satisfies (function t' -> if t = t' then Some () else None)

  let mkbinop b ((e1,_),e2) = Binop (e1,b,e2)

  (* Here is our grammar for expressions.  Note that we've
     stratified it so that we treat INT and VAR and  '('exp ')' 
     as a-expressions, and then group the TIMES and
     DIV next using b-expressions, then PLUS and MINUS using
     c-expressions, and finally LET as a d-expression, reflecting
     the relative precedence of the operators.  

     However, note that this does *not* get the associativity
     right.  In particular, this grammar makes all of the operators
     right-associative, when minus and divide should be left-
     associativie.  (Plus, times, and let don't matter since they
     are naturally associative operations.)  
  *)
  let rec aexp : (token,exp) grammar = 
    fun s -> 
      alts [
        int_p ; 
        (var_p % (fun x -> Var x)) ;
        (tok LPAREN $ exp_p $ tok RPAREN) % 
          (function ((_,e),_) -> e) 
      ] s
  and bexp : (token,exp) grammar = 
    fun s -> 
      alts [
        aexp ; 
        (aexp $ tok TIMES $ bexp) % mkbinop Times ; 
        (aexp $ tok DIV $ bexp) % mkbinop Div
      ] s
  and cexp : (token,exp) grammar = 
    fun s -> 
      alts [
        bexp ; 
        (bexp $ tok PLUS $ cexp) % mkbinop Plus ; 
        (bexp $ tok MINUS $ cexp) % mkbinop Minus
      ] s
  and dexp : (token,exp) grammar = 
    fun s -> 
      alts [ 
        cexp ; 
        (tok LET $ var_p $ tok EQ $ exp_p $ tok IN $ dexp) %
          (function (((((_,x),_),e1),_),e2) -> Let(x,e1,e2))
      ] s

  and exp_p : (token, exp) grammar = fun s -> dexp s

  let exp_parse (s:string) = 
    let ts = lex s in 
    let rs = exp_p ts in 
    let uses_all = List.filter (fun p -> snd p = []) rs in 
      match uses_all with 
        | [e] -> e
        | [] -> raise (Failure "syntax error")
        | _ -> raise (Failure "ambiguous")
end
