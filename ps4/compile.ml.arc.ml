(* Compile Cish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let temp_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

let new_temp_int() = (temp_counter := (!temp_counter) + 1; !temp_counter)
let new_temp(name:string) = (name^(string_of_int (new_temp_int()))^"t")
(* sets of variables -- Ocaml Set and Set.S *)

let temp_reset() = temp_counter:=0
let offset_counter = ref (1)
let new_offset() = (offset_counter := (!offset_counter) - 4; !offset_counter)
let offset_reset() = offset_counter:= 1

module EnvMap = Map.Make(struct
                           type t = string
                           let compare x0 x1 = 
                           String.compare x0 x1 
                         end)


(* a table of variables that we need for the code segment *)
let env : int EnvMap.t ref = ref (EnvMap.empty)
(* create a map for function env size and variables' offset *)
let rec new_var name x = 
    let key = name^x in
    if (EnvMap.mem key (!env)) then ()
    else (env := EnvMap.add key (new_offset()) (!env))

(* create a map for function env size and variables' offset *)
let create_env_fun name args stmt : unit = 
    let rec create_env_arg name args: unit = 
    (match args with
    | [] -> ()
    | x::res -> (new_var name x);(create_env_arg name res)) in

    let rec create_env_var name (stmt:Ast.stmt): unit = 
    match stmt with | (st,_) -> match st with
    | Seq(sa,sb) -> (create_env_var name sa);(create_env_var name sb)
    | Exp(e) -> (match e with | (re,_) -> (match re with
        | Assign(x,ea) -> 
            (* (new_var name x); *)
            (create_env_var name (Exp(ea),0))
        | _ -> ()))
    | For(ea,_,_,st) -> (create_env_var name (Exp(ea),0));(create_env_var name st)
    | If(_,sa,sb) -> (create_env_var name sa);(create_env_var name sb)
    | While(_,st) -> (create_env_var name st)
    | Let(id,ea,st) -> ((new_var name id);create_env_var name (Exp(ea),0);create_env_var name st)
    (* | Assign(_,exp) -> (creat_env (Exp(exp),0)) *)
    | _ -> () in
    offset_reset();create_env_arg name args;create_env_var name stmt;(new_var name "31");(new_var name "30");(env := EnvMap.add name (!offset_counter * (-1) + 1) (!env))

let rec create_env (p: Ast.program) : unit = 
    (match p with
    | [] -> ()
    | fn::rest -> (match fn with
        | Fn {name=n; args=a; body=s; pos=p} -> 
            ((create_env_fun n a s);(create_env rest))));
            (create_env_fun "printInt" [] (Exp(Int 0,0),0))


let rec compile_stmt ((s,_):Ast.stmt) (name:string) : inst list = 
  (*************************************************************)
  let int2bool() = let castfalse = new_label() in
      let endcast = new_label() in
              [Beq(R2,R0,castfalse);
              Li(R2,Int32.one);J(endcast);Label(castfalse);
              Li(R2,Int32.zero);Label(endcast)]
  in 
  let rec compile_exp ((e,_):Ast.exp) (name: string): inst list = 
      match e with
      | Int j -> [Li(R2, Word32.fromInt j)]
      | Var x -> [Lw(R2,R30,(Int32.of_int (EnvMap.find (name^x) (!env))))]
      | And(e1,e2) -> 
          (compile_exp e1 name) @int2bool()@ [Add(R29,R29,Immed (Int32.sub 0l 4l));
                Sw(R2,R29,0l)]
          @(compile_exp e2 name) @int2bool() @[Lw(R3,R29,0l);
                                            And(R2,R2,Reg R3);
                                            Add(R29,R29,Immed 4l)]
      | Or(e1,e2) -> 
        (compile_exp e1 name) @int2bool()@ [Sw(R2,R30,(Int32.of_int (EnvMap.find (name^"3") (!env))))]
        @(compile_exp e2 name) @int2bool() @[Lw(R3,R30,(Int32.of_int (EnvMap.find (name^"3") (!env))));
                                            Or(R2,R2,Reg R3)]
      | Binop(e1,b,e2) ->
          (*save e1 r3*)
          (compile_exp e1 name) @ [Add(R29,R29,Immed (Int32.sub 0l 4l));
          Sw(R2,R29,0l)]
            @(compile_exp e2 name) @[Lw(R3,R29,0l)]
          @(match b with
          | Plus -> [Add(R2,R2,Reg R3)]
          | Times -> [Mul(R2,R2,R3)]
          | Minus -> [Sub(R2,R3,R2)]
          | Div -> [Div(R2,R3,R2)]
          | Neq -> [Sne(R2,R2,R3)]
          | Eq -> [Seq(R2,R2,R3)]
          | Lt -> [Slt(R2,R3,Reg R2)]
          | Lte -> [Sle(R2,R3,R2)]
          | Gt -> [Sgt(R2,R3,R2)]
          | Gte -> [Sge(R2,R3,R2)]
          ) @ [Add(R29,R29,Immed 4l)]
      | Assign(x,e1) -> (compile_exp e1 name) @
      [Sw(R2,R30,(Int32.of_int (EnvMap.find (name^x) (!env))))]
      | Not e1 -> (compile_exp e1 name)@int2bool()@[Xor(R2,R2,Immed Int32.one)]
      | Call(funname,elist) -> 
        let rec compile_arg (args:Ast.exp list) : inst list =  
            (match args with 
            | [] -> []
            | e1::rst -> (((compile_exp e1 name)@[
                Add(R29,R29,Immed (Int32.sub 0l 4l));
                Sw(R2,R29,0l)])
            @ (compile_arg rst)))
        in
        let rec cnt_args (args:Ast.exp list) : int = 
            (match args with 
            | [] -> 0
            | e1::rst -> (4 + (cnt_args rst)))
        in
        (compile_arg elist) @
        [
        Add(R29,R29,(Immed (Int32.of_int (cnt_args(elist)-(EnvMap.find funname (!env))))));
        ]
         @ [Jal funname;
         Lw(R31,R30,(Int32.of_int (EnvMap.find (name^"31") (!env))));
         ]
  in
  match s with 
  | Exp e ->
      compile_exp e name
  | Seq(s1,s2) ->
      (compile_stmt s1 name) @ (compile_stmt s2 name)
  | If(e,s1,s2) ->
      (let else_l = new_label() in
      let end_l = new_label() in
      (compile_exp e name) @ [Beq(R2,R0,else_l)] @
      (compile_stmt s1 name) @ [J end_l;Label else_l] @
      (compile_stmt s2 name) @ [Label end_l])
  | While(e,s) ->
      (let test_l = new_label() in
      let top_l = new_label() in
      [J test_l; Label top_l] @
      (compile_stmt s  name) @
      [Label test_l] @
      (compile_exp e name) @
      [Bne(R2,R0,top_l)])
  | For(e1,e2,e3,s) ->
      (compile_stmt (Seq((Exp e1,0),(While(e2,(Seq(s,(Exp e3,0)),0)),0)),0)) name
  | Return(e) -> (compile_exp e name)@[
    Lw(R30,R29,0l);
    Add(R29,R29,Immed (Int32.of_int (EnvMap.find name (!env))))]@ 
    if name = "main" then [Add(R4,R2,Immed 0l);Add(R30,R31,Immed 0l);J("printInt")] else 
       [Jr(R31)]
  | Let(x,e1,s) -> ((compile_exp ((Assign(x,e1)),0) name)@(compile_stmt s name))

  
let rec compile (p:Ast.program) : result =
    let rec compile' (p:Ast.program): inst list = 
    match p with
    | [] -> []
    | fn::rest -> (match fn with
     | Fn {name=n; args=a; body=s; pos=_} -> ([Label n] @
      (if n="main" then 
        [Add(R29,R29,Immed (Int32.sub 0l 8l));
        ] else [])
      @
    [Sw(R30,R29,0l);
    Add(R30,R29,Immed (Int32.of_int ((EnvMap.find n (!env)) -1)));
    Sw(R31,R30,(Int32.of_int (EnvMap.find (n^"31") (!env))));] @ compile_stmt s n)@
    (compile' rest) )in
     create_env p;
    (* (EnvMap.iter (fun key value -> Printf.printf "%s: %d\n" key value) !env); *)
    {code = (compile' p);data=[]}    
let result2string (res:result) : string = 
    let code = res.code in
    let data = res.data in
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let vaR8decl x = x ^ ":\t.word 0\n" in
    let readfile f =
      let stream = open_in f in
      let size = in_channel_length stream in
      let text = Bytes.create size in
      let _ = really_input stream text 0 size in
		  let _ = close_in stream in 
      text in
	  let debugcode = readfile "print.asm" in
	    "\t.text\n" ^
	    "\t.align\t2\n" ^
	    "\t.globl main\n" ^
	    (String.concat "" strs) ^
	    "\n\n" ^
	    "\t.data\n" ^
	    "\t.align 0\n"^
	    (String.concat "" (List.map vaR8decl data)) ^
	    "\n" ^
	    Bytes.to_string debugcode
