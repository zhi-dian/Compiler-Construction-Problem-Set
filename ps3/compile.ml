(* Compile Fish AST to MIPS AST *)
open Mips

exception IMPLEMENT_ME

type result = { code : Mips.inst list;
                data : Mips.label list }

(* generate fresh labels *)
let label_counter = ref 0
let new_int() = (label_counter := (!label_counter) + 1; !label_counter)
let new_label() = "L" ^ (string_of_int (new_int()))

(* sets of variables -- Ocaml Set and Set.S *)
module VarSet = Set.Make(struct
                           type t = Ast.var
                           let compare = String.compare
                         end)

(* a table of variables that we need for the code segment *)
let variables : VarSet.t ref = ref (VarSet.empty)

(* generate a fresh temporary variable and store it in the variables set. *)
let rec new_temp() = 
    let t = "T" ^ (string_of_int (new_int())) in
    (* make sure we don't already have a variable with the same name! *)
    if VarSet.mem t (!variables) then new_temp()
    else (variables := VarSet.add t (!variables); t)

(* reset internal state *)
let reset() = (label_counter := 0; variables := VarSet.empty)

(* find all of the variables in a program and add them to
 * the set variables *)
(*add "var"before all variables to avoid crash with keyword*)
 let rec collect_vars (p : Ast.program) : unit = 
    (*************************************************************)
    match p with | (st,_) -> match st with
    | Seq(sa,sb) -> (collect_vars sa);(collect_vars sb)
    | Exp(e) -> (match e with | (re,_) -> (match re with
        | Assign(x,ea) -> (variables := VarSet.add ("var"^x) (!variables));(collect_vars (Exp(ea),0))
        | _ -> ()))
    | For(ea,_,_,st) -> (collect_vars (Exp(ea),0));(collect_vars st)
    | If(_,sa,sb) -> (collect_vars sa);(collect_vars sb)
    | While(_,st) -> (collect_vars st)
    (* | Assign(_,exp) -> (collect_vars (Exp(exp),0)) *)
    | _ -> ()
    (*************************************************************)

(* compiles a Fish statement down to a list of MIPS instructions.
 * Note that a "Return" is accomplished by placing the resulting
 * value in R2 and then doing a Jr R31.
 *)
let rec compile_stmt ((s,_):Ast.stmt) : inst list = 
    (*************************************************************)
    

    let int2bool() = let castfalse = new_label() in
        let endcast = new_label() in
                [Beq(R2,R0,castfalse);
                Li(R2,Int32.one);J(endcast);Label(castfalse);
                Li(R2,Int32.zero);Label(endcast)]
    in 
    let rec compile_exp ((e,_):Ast.exp) : inst list = 
        match e with
        | Int j -> [Li(R2, Word32.fromInt j)]
        | Var x -> [La(R2,"var"^x); Lw(R2,R2,Int32.zero)]
        | And(e1,e2) -> 
            let t = new_temp()in(compile_exp e1) @int2bool()@ [La(R3,t); Sw(R2,R3,Int32.zero)]
            @(compile_exp e2) @int2bool()@ [La(R3,t); Lw(R3,R3,Int32.zero)]
            @[And(R2,R2,Reg R3)]
        | Or(e1,e2) -> 
            let t = new_temp()in(compile_exp e1) @int2bool()@ [La(R3,t); Sw(R2,R3,Int32.zero)]
            @(compile_exp e2) @int2bool()@ [La(R3,t); Lw(R3,R3,Int32.zero)]
            @[Or(R2,R2,Reg R3)]
        | Binop(e1,b,e2) ->
            let t = new_temp()in
            (*save e1 to temp and save temp add to r3*)
            (compile_exp e1) @ [La(R3,t); Sw(R2,R3,Int32.zero)]
            @(compile_exp e2) @ [La(R3,t); Lw(R3,R3,Int32.zero)]
            @(match b with
            | Plus -> [Add(R2,R2,Reg R3)]
            | Times -> [Mul(R2,R2,R3)]
            | Minus -> [Sub(R2,R3,R2)]
            | Div -> [Div(R2,R3,R2)]
            | Neq -> [Sne(R2,R2,R3)]
            | Eq -> [Seq(R2,R2,R3)]
            | Lt -> [Slt(R2,R3,R2)]
            | Lte -> [Sle(R2,R3,R2)]
            | Gt -> [Sgt(R2,R3,R2)]
            | Gte -> [Sge(R2,R3,R2)]
            )
        | Assign(x,e1) -> (compile_exp e1) @
        [La(R3,("var"^x)); Sw(R2,R3,Int32.zero)]
        | Not e1 -> (compile_exp e1)@int2bool()@[Xor(R2,R2,Immed Int32.one)]
    in
    match s with 
    | Exp e ->
        compile_exp e
    | Seq(s1,s2) ->
        (compile_stmt s1) @ (compile_stmt s2)
    | If(e,s1,s2) ->
        (let else_l = new_label() in
        let end_l = new_label() in
        (compile_exp e) @ [Beq(R2,R0,else_l)] @
        (compile_stmt s1) @ [J end_l;Label else_l] @
        (compile_stmt s2) @ [Label end_l])
    | While(e,s) ->
        (let test_l = new_label() in
        let top_l = new_label() in
        [J test_l; Label top_l] @
        (compile_stmt s) @
        [Label test_l] @
        (compile_exp e) @
        [Bne(R2,R0,top_l)])
    | For(e1,e2,e3,s) ->
        (compile_stmt (Seq((Exp e1,0),(While(e2,(Seq(s,(Exp e3,0)),0)),0)),0))
    | Return(e) ->(compile_exp e)
        (* @[Sw(R2,R2,Int32.zero)] *)
        @[Jr(R31)]

    (*************************************************************)

(* compiles Fish AST down to MIPS instructions and a list of global vars *)
let compile (p : Ast.program) : result = 
    let _ = reset() in
    let _ = collect_vars(p) in
    let insts = (Label "fish_main") :: (compile_stmt p) in
    { code = insts; data = VarSet.elements (!variables) }

(* converts the output of the compiler to a big string which can be 
 * dumped into a file, assembled, and run within the SPIM simulator
 * (hopefully). *)
let result2string ({code;data}:result) : string = 
    let strs = List.map (fun x -> (Mips.inst2string x) ^ "\n") code in
    let var2decl x = x ^ ":\t.word 0\n" in
    "\t.text\n" ^
    "\t.align\t2\n" ^
    "\t.globl printInt\n" ^
    "\t.globl fish_main\n" ^
    "\t.globl main\n\n" ^
    "main:\n" ^
    "\tmove $s8, $31\n" ^
    "\tjal fish_main\n" ^
    "\tmove $31, $s8\n" ^
    "\tmove $a0, $2\n" ^
    "\tj printInt\n\n" ^
    "printInt:\n" ^
    "\tadd $t0, $v0, $zero\n"^
    "\tli $v0, 1\n"^
    "\tsyscall\n"^
    "\tadd $v0, $t0, $zero\n"^
    "\tjr $ra\n\n" ^
    (String.concat "" strs) ^
    "\n\n" ^
    "\t.data\n" ^
    "\t.align 0\n"^
    (String.concat "" (List.map var2decl data)) ^
    "\n"
