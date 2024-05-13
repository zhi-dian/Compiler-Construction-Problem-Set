type var = string

type primop = 
  Plus   (* add two ints *)
| Minus  (* subtract two ints *)
| Times  (* multiply two ints *)
| Div    (* divide two ints *)
| Cons   (* create a pair *)
| Fst    (* fetch the 1st component of a pair *)
| Snd    (* fetch the 2nd component of a pair *)
| Eq     (* compare two ints for equality *)
| Lt     (* compare two ints for inequality *)

type exp = 
  Int of int                    (* integer constants *)
| Var of var                    (* read value in variable *)
| Seq of exp*exp

let fun_list = ref []

let upd_fun x =
  fun_list:= !fun_list@[x]

let reset_fun = 
  fun_list := []

let rec comp (e:exp) : int list =
  match e with
  |Int x -> [3]
  |Var _ -> upd_fun 7;[4]
  |Seq(e1,e2) -> (comp e1) @ (comp e2)

let comp_all (e:exp) : int list = 
  (comp e)@(!fun_list)