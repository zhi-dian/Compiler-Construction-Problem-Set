debug

Binop:
Li t0 exp1
Li t1 exp2
Binop t0 t0 t1

Assign:
Li t0 exp

exp:
  Int of int
| Var of var
| Binop of exp * binop * exp
| Not of exp                          (* !x *)
| And of exp * exp                    (* x < y && y < z *)
| Or of exp * exp                     (* x < y || x < z *)
| Assign of var * exp                 (* x = y+42 *)

rstmt:
  Exp of exp                          (* x = 3+4; *)
| Seq of stmt * stmt                  (* x = 2*9; y = 42; *)
| If of exp * stmt * stmt             (* if (x == y) x = 42 else y = 43 *)
| While of exp * stmt                 (* while (x < y) x = x + 1; *)
| For of exp * exp * exp * stmt       (* for (x=0; x<y; x=x+1) y=y*42; *)
| Return of exp                       (* return e; *)

let compile_exp ((e,_):Ast.exp) : inst list = 
match e with
 | Int j -> [Li(R2, Word32.fromInt j)]
 | Var x -> [La(R2,x); Lw(R2,R2,zero)]
 | Binop(e1,b,e2) -> (compile_binop e1,b,e2)
 | Assign(x,e1) -> [exp2mips e1] @
 [La(R1,x), Sw(R2,R1,zero)]
 | Not e1 -> [exp2mips e1]@[Xor(R2,R2,zero)]


 let compile_binop e1 b e2 = 
 let t = new_temp()in
 (*save e1 to temp and save temp add to r3*)
 (compile_exp e1) @ [La(R3,t); Sw(R2,R3,zero)]
 @(compile_exp e2) @ [La(R3,t); Lw(R3,R3,zero)]
 @(match b with
 | Plus -> [Add(R2,R2,Reg R3)]
 | Times -> [Mul(R2,R2,R3)]
 | Minus -> [Sub(R2,R3,R2)]
 | Div -> [Div(R2,R2,R3)]
 | Neq -> [Sne(R2,R2,R3)]
 | Eq -> [Sne(R2,R2,R3)]
 | Lt -> [Slt(R2,R2,R3)]
 | Lte -> [Sle(R2,R2,R3)]
 | Gt -> [Sgt(R2,R2,R3)]
 | Gte -> [Sge(R2,R2,R3)]
 | And -> [And(R2,R2,Reg R3)]
 | Or -> [Or(R2,R2,Reg R3)]
 )

 let rec stmt2mips(s:stmt):inst list =
  match s with
  | Exp e ->
  exp2mips e
  | Seq(s1,s2) ->
  (stmt2mips s1) @ (stmt2mips s2)
  | If(e,s1,s2) ->
  (let else_l = new_label() in
  let end_l = new_label() in
  (exp2mips e) @ [Beq(R2,R0,else_l)] @
  (stmt2mips s1) @ [J end_l;Label else_l] @
  (stmt2mips s2) @ [Label end_l])
  | While(e,s) ->
    (let test_l = new_label() in
    let top_l = new_label() in
    [J test_l, Label top_l] @
    (stmt2mips s) @
    [Label test_l] @
    (exp2mips e) @
    [Bne(R2,R0,top_l)])
   | For(e1,e2,e3,s) ->
    stmt2mips(Seq(Exp e1,While(e2,Seq(s,Exp e3))))