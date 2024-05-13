open Mlish_ast
open Scish_ast

module ML = Mlish_ast
module S = Scish_ast



exception ImplementMe
let type_error(s:string) = (print_string s; raise ImplementMe)

module StringtoVal = struct
  type t = (string*S.exp) list
end

let rec substitue ((sube,_):Mlish_ast.exp) (key:Mlish_ast.var) ((e,_):Mlish_ast.exp):Mlish_ast.exp=
match e with
| Var x -> 
  if key=x 
    then 
      (sube,0)
    else 
      (Var x,0)
| Fn(x,e) -> 
  if key=x 
    then 
      (Fn(x,e),0)
    else 
      (Fn(x,substitue (sube,0) key e),0)
| App(e1,e2) -> 
  (App(substitue (sube,0) key e1,substitue (sube,0) key e2),0)
| If(e1,e2,e3) -> 
  (If(substitue (sube,0) key e1,substitue (sube,0) key e2,substitue (sube,0) key e3),0)
| Let(x,e1,e2) -> 
  if key=x 
    then 
      (Let(x,e1,e2),0)
    else 
      (Let(x,substitue (sube,0) key e1,substitue (sube,0) key e2),0)
| PrimApp(p,e_list) -> 
  let rec sub_lst key sube e_list:Mlish_ast.exp list=
    match e_list with
    | [] -> []
    | e::res -> [substitue (sube,0) key e]@(sub_lst key sube res)
  in
  (PrimApp(p,sub_lst key sube e_list),0)

  let rec expr2string ((e, _): ML.exp) : string =
    match e with
    | Var var -> "Var " ^ var
    | PrimApp (prim, exps) -> "PrimApp (" ^ prim2string prim ^ ", [" ^ String.concat "; " (List.map expr2string exps) ^ "])"
    | Fn (v, e) -> "Fn (" ^ v ^ ", " ^ expr2string e ^ ")"
    | App (e1, e2) -> "App (" ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ")"
    | If (e1, e2, e3) -> "If (" ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ", " ^ expr2string e3 ^ ")"
    | Let (v, e1, e2) -> "Let (" ^ v ^ ", " ^ expr2string e1 ^ ", " ^ expr2string e2 ^ ")"
  and prim2string prim = 
    match prim with
    | Int n -> "Int " ^ string_of_int n
    | Bool b -> "Bool " ^ string_of_bool b
    | Unit -> "Unit"
    | Plus -> "Plus"
    | Minus -> "Minus"
    | Times -> "Times"
    | Div -> "Div"
    | Eq -> "Eq"
    | Lt -> "Lt"
    | Pair -> "Pair"
    | Fst -> "Fst"
    | Snd -> "Snd"
    | Nil -> "Nil"
    | Cons -> "Cons"
    | IsNil -> "IsNil"
    | Hd -> "Hd"
    | Tl -> "Tl"

let rec compile_exp (e:ML.exp) : S.exp = 
  let rec compile_e ((e,_):Mlish_ast.exp) : Scish_ast.exp= 

  let compile_prim (p:Mlish_ast.prim) (e_list:Mlish_ast.exp list): S.exp = 
    match p with 
    | Int(i) -> Int(i)
    | Bool(b) -> (match b with
      | true -> Int(1)
      | false -> Int(0))
    | Unit -> raise ImplementMe
    | Plus ->   PrimApp(Plus,List.map compile_e e_list)
    | Minus ->  PrimApp(Minus,List.map compile_e e_list)
    | Times ->  PrimApp(Times,List.map compile_e e_list)
    | Div ->    PrimApp(Div,List.map compile_e e_list)
    | Eq ->     PrimApp(Eq,List.map compile_e e_list)
    | Lt ->     PrimApp(Lt,List.map compile_e e_list)
    | Pair ->  PrimApp(Cons,List.map compile_e e_list)
    | Fst ->  PrimApp(Fst,List.map compile_e e_list)
    | Snd ->  PrimApp(Snd,List.map compile_e e_list) 
    | Nil -> PrimApp(Cons,[Int(0);Int(0)])
    | Cons ->   
      (match e_list with
      | e1::e2::[] -> 
        let com_e1 = compile_e e1 in
        let com_e2 = compile_e e2 in
        PrimApp(Cons,[PrimApp(Plus,[Int(1);PrimApp(Fst,[com_e2])]);
                      PrimApp(Cons,[com_e1;PrimApp(Snd,[com_e2])])])
      | _-> raise ImplementMe)
    | IsNil -> 
      (match e_list with
      | e1::[] -> 
        let com_e1 = compile_e e1 in 
          If(PrimApp(Eq,[PrimApp(Fst,[com_e1]);Int(0)]),Int(1),Int(0))
      | _ -> raise ImplementMe)
        
    | Hd ->  PrimApp(Fst,[PrimApp(Snd,List.map compile_e e_list)])
    | Tl -> 
      (match e_list with
      | e1::[] -> 
        (* print_endline (expr2string e1); *)
        let com_e1 = compile_e e1 in
          If(PrimApp(Eq,[PrimApp(Fst,[com_e1]);Int(1)]),
          PrimApp(Snd,[com_e1]),
          (match e1 with
            | (ML.PrimApp(ML.Cons,[e1a;(ML.PrimApp(ML.Cons,[e2a;e2b]),_)]),_) ->
              (compile_e (ML.PrimApp(ML.Cons,[(ML.PrimApp(Minus,[e1a;(ML.PrimApp(Int(1),[]),0)]),0);e2b]),0))
            | _-> raise ImplementMe
          )
          )
      | _ -> raise ImplementMe)

  in
  match e with
  | Var x -> Var(x)
  | Fn(x,e) -> 
    Lambda(x,compile_e e)
  | App(e1,e2) -> App(compile_e e1,compile_e  e2)
  | If(e1,e2,e3) -> If(compile_e  e1,compile_e  e2,compile_e  e3)
  | Let(x,e1,e2) ->
      let res = 
        substitue e1 x e2
      in compile_e  res 
  | PrimApp(p,e_list) -> compile_prim p e_list

in 
(* print_endline (expr2string e); *)
(compile_e e)