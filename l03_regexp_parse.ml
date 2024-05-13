(* This file demonstrates how you might not only do matching, but
   extracting information...i.e., lexing. *)

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

(*******************************************)
(* Our interface for the basic lexing      *)
(*******************************************)
module type LEX = 
sig
  (* an ['a regexp] matches a string and returns an ['a] value *)
  type 'a regexp 

  (* [ch c] matches ["c"] and returns ['c'] *)
  val ch : char -> char regexp 

  (* [eps] matches [""] and returns [()] *)
  val eps : unit regexp

  (* [void] never matches (so never returns anything) *)
  val void : 'a regexp

  (* [r1 ++ r2] matches [s] and returns [v] if [r1] matches [s] and
     returns [v], or else [r2] matches [s] and returns [v]. *)
  val (++) : 'a regexp -> 'a regexp -> 'a regexp

  (* [r1 $ r2] matches [s] and returns [(v1,v2)] if [s = s1 ^ s2]
     and [r1] matches [s1] and returns [v1], and [r2] matches [s2]
     and returns [v2]. *)
  val ($) : 'a regexp -> 'b regexp -> ('a * 'b) regexp

  (* [star r] matches [s] and returns the list [vs] if either
     [s = ""] and [vs = []], or else [s = s1 ^ s2] and [vs = v1::v2]
     and [r] matchs s1 and returns v1, and [star r] matches [s2] and
     returns [v2]. *)
  val star : 'a regexp -> ('a list) regexp

  (* [r % f] matches [s] and returns [f(w)] 
     if [r] matches [s] and returns [w] *)
  val (%) : 'a regexp -> ('a -> 'b) -> 'b regexp

  (* [lex r s] tries to match [s] against [r] and returns the list
     of all values that we can get out of the match. *)
  val lex : 'a regexp -> string -> 'a list
end

(****************************************************)
(* A functor for extending a basic [LEX] module.    *)
(****************************************************)
module ExtendLex(L : LEX) = 
struct
  include L

  (* matches one or more *)
  let plus(r: 'a regexp) : ('a list) regexp = 
    (r $ (star r)) % cons 

  (* when we want to just return a value and 
     ignore the values we get out of r. *)
  let (%%) (r:'a regexp) (v:'b) : 'b regexp = 
    r % (fun _ -> v) 

  (* optional match *)
  let opt(r:'a regexp) : 'a option regexp = 
    (r % (fun x -> Some x)) ++ (eps %% None);;

  let alts (rs: ('a regexp) list) : 'a regexp = 
    List.fold_right (++) rs void 

  let cats (rs: ('a regexp) list) : ('a list) regexp = 
    List.fold_right (fun r1 r2 -> (r1 $ r2) % cons) rs 
      (eps % (fun _ -> [])) 

  (* Matches any digit *)
  let digit : char regexp = 
    alts (List.map (fun i -> ch (char_of_int (i + (int_of_char '0'))))
            [0;1;2;3;4;5;6;7;8;9])

  (* Matches 1 or more digits *)
  let natural : int regexp = 
    (plus digit) %
    (List.fold_left (fun a c -> a*10 + (int_of_char c) - (int_of_char '0')) 0)

  (* Matches a natural or a natural with a negative sign in front of it *)
  let integer : int regexp = 
    natural ++ (((ch '-') $ natural) % (fun (_,n) -> -n)) 

  (* Generate a list of numbers [i,i+1,...,stop] -- assumes i <= stop *)
  let rec gen(i:int)(stop:int) : int list = 
    if i > stop then [] else i::(gen (i+1) stop)

  (* Matches any lower case letter *)
  let lc_alpha : char regexp =
    let chars = List.map char_of_int (gen (int_of_char 'a') (int_of_char 'z')) in
      alts (List.map ch chars)

  (* Matches any upper case letter *)
  let uc_alpha : char regexp = 
    let chars = List.map char_of_int (gen (int_of_char 'A') (int_of_char 'Z')) in
      alts (List.map ch chars)

  (* Matches an identifier a la Ocaml:  must start with a lower case letter, 
     followed by 1 or more letters (upper or lower case), an underscore, or a digit. *)
  let identifier : string regexp = 
    (lc_alpha $ (star (alts [lc_alpha; uc_alpha; ch '_'; digit]))) %
    (fun (c,s) -> implode (c::s))

  type token = 
        INT of int | ID of string | LET | IN | PLUS | TIMES | MINUS | DIV | LPAREN
      | RPAREN | EQ ;;

  let keywords = [ ("let",LET) ; ("in",IN) ]

  (* here are the regexps for our little ML language *)
  let token_regexps = [
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

  (* so we can define a regexp to match any legal token *)
  let token = alts token_regexps ;;

  (* white space *)
  let ws = (plus (alts [ch ' ' ; ch '\n' ; ch '\r' ; ch '\t'])) %% () ;;

  (* document -- zero or more tokens separated by one or more white spaces *)
  let doc : token list regexp = 
    ((opt ws) $ ((star ((token $ ws) % fst)) $ (opt token))) % 
    (fun p -> let (_,(ts,topt)) = p in 
        match topt with 
          | None -> ts
          | Some t -> ts @ [t])

end

module Lex = 
struct
  (* This definition is similar to the matcher we had when
     we returned a set of lists of unconsumed characters.
     The only difference is that here, an ['a regexp] will
     return not only unconsumed characters, but also an ['a]
     value. 

     The only problem with this is that it will loop forever
     on certain regular expressions (e.g., (star eps)).
  *)
  type 'a regexp = char list -> ('a * char list) list 

  let ch(c:char) : char regexp = 
    function 
      | c'::rest -> if c = c' then [(c,rest)] else []
      | _ -> []

  let eps : unit regexp = fun s -> [((), s)] 

  let void : 'a regexp = fun s -> [] 

  let (++)(r1 : 'a regexp) (r2: 'a regexp) : 'a regexp = 
    fun s -> (r1 s) @ (r2 s) 

  let ($)(r1: 'a regexp) (r2:'b regexp) : ('a * 'b) regexp = 
    fun s -> 
      List.fold_right 
        (function (v1,s1) -> fun res -> 
           (List.fold_right 
              (function (v2,s2) -> 
                fun res -> ((v1,v2),s2)::res) (r2 s1) res)) (r1 s) [] 

  let (%) (r:'a regexp) (f:'a -> 'b) : 'b regexp = 
    fun s -> 
      List.map (function (v,s') -> (f v,s')) (r s) 

  let rec star(r:'a regexp) : ('a list) regexp = 
    fun s -> (((r $ (star r)) % cons) ++ (eps % (fun _ -> []))) s 

  let lex (r: 'a regexp) (s:string) : 'a list = 
    let results = r (explode s) in
    let uses_all = List.filter (fun p -> snd p = []) results in 
      List.map fst uses_all 
end

module ExtendedLex = ExtendLex(Lex)

