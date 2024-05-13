(* TODO:  your job is to map ScishAst expressions to CishAst functions. 
   The file sample_input.scish shows a sample Scish expression and the
   file sample_output.cish shows the output I get from my compiler.
   You will want to do your own test cases...
 *)

exception Unimplemented

exception UnboundError


open Scish_ast
open Cish_ast

(* don't consider name overlap, the newer value will overwrite old one *)
module StringIntMap = struct
    type t = (string * int) list

    let empty = []

    let add (map : t) (key : string) (value : int) : t =
    (key, value) :: map

end

let inc map = List.map (fun (key, value) -> (key, value + 1)) map

let rec lookup (map : StringIntMap.t) (key : string) : int option =
  match map with
  | [] -> None
  | (k, v) :: rest -> 
    if k = key then Some v
    else lookup rest key

let temp_var  = ref (-1)

let new_temp unit = temp_var:=!temp_var + 1;"t"^(string_of_int !temp_var)

let fun_list = ref []

let compile_var var (env:StringIntMap.t): Cish_ast.stmt = 
  let rec compile_lookup (d:int): Cish_ast.exp = 
    if d = 0 then (Binop((Var "dynenv",0),Plus,(Int 0,0)),0)
    else (Load(Binop((compile_lookup (d-1)),Plus,(Int 4,0)),0),0)
  in
  match (lookup env var) with
  |None -> raise UnboundError
  |Some depth ->     
    (Exp(Assign("result",
    (Load(compile_lookup depth),0)
    ),0),0)

let rec make_seq (s:Cish_ast.stmt list): Cish_ast.stmt = 
  match s with
  | s1::[] -> s1
  | s1::res -> (Seq(s1,(make_seq res)),0)
  | [] -> raise UnboundError

let rec compile_e (e:Scish_ast.exp) (env:StringIntMap.t): Cish_ast.stmt = 
  let rec upd_fun_list (fn:Cish_ast.func) : unit =
    fun_list := (!fun_list) @ [fn] in
  let rec create_fun (fn_name:var) (e:Scish_ast.exp) (env:StringIntMap.t): Cish_ast.func = 
      (* let fn_name = new_temp() in  *)
      Fn {name = fn_name;args = ["dynenv"];body = 
        (Let("result",((Int 0),0), 
          (Seq((compile_e e env),(Return((Var "result"),0),0)),0)),0)
      ;pos=0} in 
  let compile_primop (op:Scish_ast.primop) (explist:Scish_ast.exp list) (env:StringIntMap.t) : Cish_ast.stmt = 
    (
      match op with 
      (* | _ -> (Exp(Int 0,0),0) *)
      | Plus -> let t = new_temp() in 
      (Let(t,(Int 0,0),
          (match explist with 
          |x1::x2::[] -> 
            (Seq(
              (compile_e x1 env),(Seq(
                (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                  (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Plus,(Var "result",0)),0)),0),0))
                ,0))
              ,0))
            ,0)
          |_ -> raise UnboundError)),0)
      | Minus-> let t = new_temp() in 
      (Let(t,(Int 0,0),
      (match explist with 
        |x1::x2::[] -> 
          (Seq(
            (compile_e x1 env),(Seq(
              (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Minus,(Var "result",0)),0)),0),0))
              ,0))
            ,0))
          ,0)
        |_ -> raise UnboundError)),0)
      | Times-> let t = new_temp() in 
      (Let(t,(Int 0,0),
      (match explist with 
        |x1::x2::[] -> 
          (Seq(
            (compile_e x1 env),(Seq(
              (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Times,(Var "result",0)),0)),0),0))
              ,0))
            ,0))
          ,0)
        |_ -> raise UnboundError)),0)
      | Div  -> let t = new_temp() in 
      (Let(t,(Int 0,0),
      (match explist with 
        |x1::x2::[] -> 
          (Seq(
            (compile_e x1 env),(Seq(
              (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Div,(Var "result",0)),0)),0),0))
              ,0))
            ,0))
          ,0)
        |_ -> raise UnboundError)),0)
      | Cons -> let t = new_temp() in 
      (Let(t,(Int 0,0),
        (match explist with 
        |x1::x2::[] -> 
          (Seq((Exp(Assign((t),(Malloc((Int 8,0)),0)),0),0),(Seq(
            (compile_e x1 env),(Seq(
              (Exp(Store((Binop((Var t,0),Plus,(Int 0,0)),0),(Var "result",0)),0),0),(Seq(
              (compile_e x2 env),(Seq(
              (Exp(Store((Binop((Var t,0),Plus,(Int 4,0)),0),(Var "result",0)),0),0),
              (Exp(Assign("result",(Var t,0)),0),0)),0))
              ,0))
            ,0))
          ,0)),0)
        |_ -> raise UnboundError)),0)
      | Fst  ->         
        (match explist with 
        |x1::[] -> 
          (make_seq [
            (compile_e x1 env);
            (Exp((Assign("result",(Load((Binop((Var "result",0),Plus,(Int 0,0)),0)),0)),0)),0)
            ])
        |_ -> raise UnboundError)
      | Snd  ->          
        (match explist with 
        |x1::[] -> 
          (make_seq [
            (compile_e x1 env);
            (Exp((Assign("result",(Load((Binop((Var "result",0),Plus,(Int 4,0)),0)),0)),0)),0)
            ])
        |_ -> raise UnboundError)
      | Eq   -> let t = new_temp() in 
      (Let(t,(Int 0,0),
        (match explist with 
        |x1::x2::[] -> 
          (Seq((Exp(Assign(("result"),(Malloc((Int 8,0)),0)),0),0),(Seq(
            (compile_e x1 env),(Seq(
              (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Eq,(Var "result",0)),0)),0),0))
              ,0))
            ,0))
          ,0)),0)
        |_ -> raise UnboundError)),0)
      | Lt   -> let t = new_temp() in 
      (Let(t,(Int 0,0),
        (match explist with 
        |x1::x2::[] -> 
          (Seq((Exp(Assign(("result"),(Malloc((Int 8,0)),0)),0),0),(Seq(
            (compile_e x1 env),(Seq(
              (Exp(Assign(t,(Var "result",0)),0),0),(Seq(
                (compile_e x2 env),(Exp(Assign("result",(Binop((Var t,0),Lt,(Var "result",0)),0)),0),0))
              ,0))
            ,0))
          ,0)),0)
        |_ -> raise UnboundError)),0)
    ) in
  
  let rec compile_lambda var (e:Scish_ast.exp) (env:StringIntMap.t): Cish_ast.stmt =  
    (let t = new_temp() in let sub_env = (StringIntMap.add (inc env) var 0) in 
      ((upd_fun_list (create_fun t e sub_env)); 
          (make_seq [
            (Exp((Assign("result", (Malloc((Int 8,0)),0)),0)),0);
            (Exp((Store((Binop((Var "result", 0),Plus,(Int 0,0)),0),(Var t,0)),0)),0);
            (Exp((Store((Binop((Var "result", 0),Plus,(Int 4,0)),0),(Var "dynenv",0)),0)),0);
            (* (Exp(Int 0,0),0); *)
          ]))
        (* Exp((Assign("result", (Malloc((Int 8,0)),0)),0)) *)
    ) in
  let rec compile_app (e1:Scish_ast.exp) (e2:Scish_ast.exp) (env:StringIntMap.t): Cish_ast.stmt =
    (match (e1,e2) with 
    (* | (_,_) -> raise UnboundError *)
    |(e1,e2) ->
      let t1 = new_temp() in 
      let t2 = new_temp() in
      let t3 = new_temp() in 
      (
          Let(t1,(Int 0,0),(
            Let(t2,(Int 0,0),(
              Let(t3,(Int 0,0),
              (make_seq [
                (compile_e e1 env);
                (Exp((Assign(t1,(Load((Var "result",0)),0)),0)),0);
                (Exp((Assign(t2,(Load((Binop((Var "result",0),Plus,(Int 4,0)),0)),0)),0)),0);
                (compile_e e2 env);
                (Exp((Assign(t3,(Malloc((Int 8,0)),0)),0)),0);
                (Exp((Store((Binop((Var t3, 0),Plus,(Int 0,0)),0),(Var "result",0)),0)),0);
                (Exp((Store((Binop((Var t3, 0),Plus,(Int 4,0)),0),(Var t2,0)),0)),0);

                (Exp((Assign("result",(Call((Var t1,0),[(Var t3,0)]),0)),0)),0);
              ]))
            ,0))
          ,0))
      ,0)
  (* | _ -> print_string "is it yours";raise UnboundError *)
  )in
  let rec compile_if (e1:Scish_ast.exp) (e2:Scish_ast.exp) (e3:Scish_ast.exp) (env:StringIntMap.t): Cish_ast.stmt = 
    (
      make_seq [
        compile_e e1 env;
        (If((Var "result",0),compile_e e2 env,compile_e e3 env),0)
      ]
    ) in
  match e with
  | Int x -> 
    (Exp(Assign("result",
    ((Int x),0)
    ),0),0)
  
  | Var var -> compile_var var env
  | PrimApp (pop, explist) -> compile_primop pop explist env
  | Lambda (var,e) -> compile_lambda var e env
  | App (e1,e2) -> compile_app e1 e2 env
  | If (e1,e2,e3) -> compile_if e1 e2 e3 env

  
let rec compile_exp (e:Scish_ast.exp) : Cish_ast.program = 
  let compile_main (e:Scish_ast.exp) : Cish_ast.program = 
    [Fn {name="main";args=[];body = 

      (Let("dynenv",(Int 0,0),(Let("result",(Int 0,0), 
        (Seq((compile_e e []),(Return(Var "result",0),0)),0)
      ),0)),0)

    ;pos=0}]
  in 
  let main_fn = (compile_main e)
  in
  main_fn@(!fun_list)


