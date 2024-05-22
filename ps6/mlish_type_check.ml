open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)


module StringtoType = struct
  type t = (var*Mlish_ast.tipe) list
end

type tenv = (var*tipe_scheme) list


let rec tipe2str (t:tipe):string = 
  match t with
    Tvar_t tvar -> "'" ^ tvar
  | Int_t -> "int"
  | Bool_t -> "bool"
  | Unit_t -> "unit"
  | Fn_t (t1, t2) -> "(" ^ tipe2str t1 ^ ") -> (" ^ tipe2str t2 ^ ")"
  | Pair_t (t1, t2) -> "(" ^ tipe2str t1 ^ ") * (" ^ tipe2str t2 ^ ")"
  | List_t t -> "(" ^ tipe2str t ^ ") list"
  | Guess_t tr ->
      match !tr with
      | None -> "(None)"
      | Some t -> tipe2str t

let tsci2str tsci = 
  let lst2str lst =
    List.fold_left (fun acc a -> (a^";"^acc)) "" lst in
  match tsci with
  Forall(tlst,t) -> ((lst2str tlst)^": "^(tipe2str t))
(* let rec tenv2str (env: tenv):string=
  match env with
  | (tv,tsci)::res ->  *)

let tenv2str(e:tenv) = 
    List.fold_left (fun acc (v,a) -> (v^":"^(tsci2str a)^"||"^acc)) "" e
let rec expr2string ((e, _): exp) : string =
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


let extend (env:tenv) (x:Mlish_ast.var) (t:Mlish_ast.tipe_scheme): tenv= 
  [(x,t)]@env

let rec lookup (env:tenv) (x:Mlish_ast.var): Mlish_ast.tipe_scheme=
  match env with
  | [] -> type_error("variable not found")
  | (xa,ta)::res -> 
    (* print_endline xa; *)
    if xa = x then ta else (lookup res x)

let guess() = Guess_t(ref None)

let rec is_equal (t1:tipe) (t2:tipe): bool=
  match t1,t2 with
  | Int_t, Int_t -> true
  | Bool_t, Bool_t -> true
  | Unit_t, Unit_t -> true
  | Guess_t(t1'), Guess_t(t2') -> 
      (* if t1'=t2' then true else *)
      (match !t1', !t2' with
        | Some a, Some b -> is_equal a b
        | None, None -> false
        | _, _ -> false)
  | Fn_t(t1a, t1b), Fn_t(t2a, t2b) -> is_equal t1a t2a && is_equal t1b t2b
  | Pair_t(t1a, t1b), Pair_t(t2a, t2b) -> is_equal t1a t2a && is_equal t1b t2b
  | List_t(t1), List_t(t2) -> is_equal t1 t2
  | _, _ -> false


(* HM Algorithm *)

let rec find_tvar (b:(tvar*tipe) list) (tv:tvar):tipe=
  match b with
  | (tv1,t)::res -> (if tv1=tv then
        t
      else
        (find_tvar res tv))
  | _ -> type_error "tvar missing"

(*b: tvar and its corresponding new guess
  change all the free vars to a new guesses*)
let rec substitude (b:(tvar*tipe)list) (t:tipe):tipe = 
  match t with 
  | Tvar_t(tv) -> 
    let new_guess = find_tvar b tv in
    new_guess
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t(t1,t2) -> Fn_t(substitude b t1,substitude b t2)
  | Pair_t(t1,t2) -> Pair_t(substitude b t1,substitude b t2)
  | List_t(t1) -> List_t(substitude b t1)
  | Guess_t(t1) -> 
    match !t1 with
    | Some t1' -> 
      (* t1:=Some (substitude b t1');t THIS IS WRONG*)
      (substitude b t1')
    | None -> t
(*is there situation when a free variable exits inside a guess?*)

let instantiate s:tipe=
  match s with
  | Forall(vs,t) ->
    let b = List.map (fun a -> (a,guess())) vs
    in
    substitude b t

let rec guess_of_tipe t:tipe list=
  match t with 
  | Tvar_t(_)| Int_t| Bool_t| Unit_t -> []
  | Fn_t(t1,t2) -> (guess_of_tipe t1)@(guess_of_tipe t2)
  | Pair_t(t1,t2) -> (guess_of_tipe t1)@(guess_of_tipe t2)
  | List_t(t1) -> guess_of_tipe t1
  | Guess_t(t1) -> 
    match !t1 with
    | Some t1' -> guess_of_tipe t1'
    | None -> [t]

(*simulate set on guess list, cannot construct guess set because no compare rule*)
let rec check_exit a_list (element:tipe)=
  match element with
  | Guess_t(element') ->
    (match a_list with
    | [] -> false
    | Guess_t(a)::res -> if a==element' then true else check_exit res element
    | _ -> type_error "only guesses in the list but find other type")
  | _ -> type_error "only guesses in the list but find other type"

let rec union list1 list2 = 
  match list1 with
  | ele::res -> if check_exit list2 ele then union res list2 else ele::(union res list2)
  | [] -> list2

let rec minus list1 list2 =
  match list1 with
  | [] -> []
  | a::res -> if check_exit list2 a then 
    (minus res list2) else
    a::(minus res list2)

let rec guess_of_env s = 
  match s with
  | Forall(_,t) -> guess_of_tipe t

  (*t is a guess for sure, None?*)
let rec find_var_ref (gx_vs:(tipe*var)list) (t:tipe):var option= 
  match t with 
  | Guess_t(t') -> 
    (match gx_vs with 
    | (Guess_t(gs'), fvar)::res -> if gs' == t' then Some fvar else (find_var_ref res t)
    | [] -> None
    | _ -> type_error "error type in gx_vs")
  | _ -> type_error "should be guess"
(*change all the guesses to free variables*)
let rec subst_guess (gx_vs:(tipe*var)list) (t:tipe):tipe = 
  match t with 
  | Tvar_t(tv) -> Tvar_t(tv)
  | Int_t -> Int_t
  | Bool_t -> Bool_t
  | Unit_t -> Unit_t
  | Fn_t(t1,t2) -> Fn_t(subst_guess gx_vs t1,subst_guess gx_vs t2)
  | Pair_t(t1,t2) -> Pair_t(subst_guess gx_vs t1,subst_guess gx_vs t2)
  | List_t(t1) -> List_t(subst_guess gx_vs t1)
  | Guess_t(t1) -> 
    match !t1 with
    | Some t1' -> 
      (* t1:= Some (subst_guess gx_vs t1');t THIS IS WRONG*)
      (subst_guess gx_vs t1')
    | None -> 
      match find_var_ref gx_vs t with
      | Some tvar -> 
        Tvar_t(tvar)
      | None -> t
(*avoid duplication: in subst_guess, only the first guess will be subst
   duplication might happen if a tipe has several guess using the same reference*)
let generalize(e:tenv)(t:tipe):tipe_scheme =
  let t_gs = guess_of_tipe t in
  let env_list_gs =
    List.map (fun (x,s) -> guess_of_env s) e in
  let env_gs = List.fold_left union [] env_list_gs in
  let diff = minus t_gs env_gs in
  let gs_vs =
    List.map (fun g -> (g,ppfreshtvar())) diff in
  let tc = subst_guess gs_vs t
  in
  Forall(List.map snd gs_vs, tc) 

let type_check_exp (e:Mlish_ast.exp) : tipe =
  let rec tc (env:tenv) (e:Mlish_ast.exp) : tipe= 
    let rec check_guess (t1:Mlish_ast.tipe): tipe = (*return normal type or empty guess*)
      (match t1 with
      | Guess_t(t1') -> 
        (match !t1' with 
        | Some t1' -> check_guess t1'
        | None -> t1)
      | List_t(t1') -> List_t(check_guess t1')
      | Fn_t(t1',t2') -> Fn_t(check_guess t1',check_guess t2')
      | Pair_t(t1',t2') -> Pair_t(check_guess t1',check_guess t2')
      | ti -> ti)
    in
    let rec occurs (t1:tipe option ref) t2:bool=
      match t2 with
      | Guess_t(t2') -> 
        (match !t2' with
        | Some t2'' -> (if t1==t2' then true else occurs t1 t2'')
        | None -> t1==t2')
      | Pair_t(t2a,t2b) -> (occurs t1 t2a)||(occurs t1 t2b)
      | Fn_t(t2a,t2b) -> (occurs t1 t2a)||(occurs t1 t2b)
      | List_t(t) -> (occurs t1 t)
      | _ -> false
    in
    let rec unify (t1:tipe) (t2:tipe):bool =
      (* let t1 = check_guess t1 in
      let t2 = check_guess t2 in *)
      (print_endline ("unify "^(tipe2str t1)^" and "^(tipe2str t2)));
      if (is_equal t1 t2) then (true) else
      (match t1,t2 with
      | Guess_t(t1'), _ -> 
        (match !t1' with
        | None ->
          (match t2 with
            | Guess_t(t2') -> if t2'==t1' then true else 
              (match !t2' with
              | None -> if t2'==t1' then true else (t1':=Some t2;true)
              | Some t2'' -> unify t2 t1)
            | _ -> if occurs t1' t2 then type_error "graph equality failure" else (t1':=Some t2;true))
        | Some t1'' -> 
          (* print_endline (tipe2string t1''); *)
          unify t1'' t2)
      | _, Guess_t(_) -> unify t2 t1
      | Int_t, Int_t -> true
      | Fn_t(t1a,t1b), Fn_t(t2a,t2b) -> 
        unify t1a t2a && unify t1b t2b
      | Pair_t(t1a,t1b), Pair_t(t2a,t2b) -> 
        unify t1a t2a && unify t1b t2b
      | List_t(t1a), List_t(t2a) ->
        unify t1a t2a
      | _,_ -> type_error("unify"^(tipe2str t1)^"and"^(tipe2str t2)))
    in
    let rec tc_PrimApp (env:tenv)(p:Mlish_ast.prim) (e_list:Mlish_ast.exp list): tipe = 
      let check_binary_int (env:tenv)(e_list:Mlish_ast.exp list)(return_tipe:tipe): tipe = 
        match e_list with 
        | e1::e2::[] -> 
          (match (check_guess (tc env e1),check_guess (tc env e2)) with
          | Int_t,Int_t -> return_tipe
          | Guess_t(t1),Guess_t(t2) -> t1:=Some Int_t;t2:=Some Int_t;return_tipe
          | Guess_t(t1),Int_t -> t1:=Some Int_t;return_tipe
          | Int_t,Guess_t(t2) -> t2:=Some Int_t;return_tipe
          | _ -> type_error("primapp"))
          (* if check_guess env (tc env e1) (tc env e2) then return_tipe else type_error("") *)
        | _ -> type_error("primapp")
      in
      match p with 
      | Int(i) -> Int_t
      | Bool(b) -> Bool_t
      | Unit -> Unit_t
      | Plus ->   (check_binary_int env e_list Int_t)
      | Minus ->  (check_binary_int env e_list Int_t)
      | Times ->  (check_binary_int env e_list Int_t)
      | Div ->    (check_binary_int env e_list Int_t)
      | Eq ->     (check_binary_int env e_list Bool_t)
      | Lt ->     (check_binary_int env e_list Bool_t)
      | Pair ->   
          let res = (match e_list with 
          | e1::e2::[] -> 
            Pair_t(tc env e1,tc env e2)
          | _ -> type_error("primapp")) in 
          (* print_string (tipe2str res); *)
          res
      | Fst ->
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          (* (match tc env e1 with *)
          | Pair_t(t1,_) -> t1
          | Guess_t(t) -> let g1 = guess() in let g2 = guess() in t:= Some (Pair_t(g1,g2));g1
          | _ -> type_error("primapp"))
        | _ -> type_error("primapp"))
      | Snd ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          (* (match tc env e1 with *)
          | Pair_t(t1,_) -> t1
          | Guess_t(t) -> let g1 = guess() in let g2 = guess() in t:= Some (Pair_t(g1,g2));g2
          | _ -> type_error("primapp"))
        | _ -> type_error("primapp"))
      | Nil -> let g = guess() in if e_list=[] then List_t(g) else type_error("primapp")
      | Cons ->   
        (match e_list with 
        | e1::e2::[] -> 
          (match (check_guess (tc env e1),check_guess (tc env e2)) with
          | Guess_t(t1),Guess_t(t2) -> let g = guess() in t1:=Some g;t2:=Some (List_t(g));List_t(g)
          (*how does list guarantee t1, t2 has same type if they are all none?*)
          | Guess_t(t1),t2 -> 
              (match t2 with
              |List_t(t2') -> t1:=Some t2';t2
              | _ -> type_error("primapp"))
          | t1,Guess_t(t2) -> t2:=Some (List_t(t1));List_t(t1)
          | t1,List_t(t2) -> (match (check_guess t2) with
            | Guess_t(t2') -> t2':= Some t1;List_t(t1)
            | t2' -> if t1=t2' then List_t(t1) else type_error("primapp"))
          | _,_ -> type_error("primapp"))
        | _ -> type_error("primapp"))
      | IsNil ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(_) -> Bool_t
          | Guess_t(t) -> let g1 = guess() in t:= Some (List_t(g1));Bool_t
          | _ -> type_error("primapp"))
        | _ -> type_error("primapp"))
      | Hd ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(t) -> t
          | Guess_t(t) -> let g = guess() in t:=Some (List_t(g));g
          | _ -> type_error("primapp"))
        | _ -> type_error("primapp"))
      | Tl ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(t) -> List_t(t)
          | Guess_t(t) -> let g = guess() in t:=Some (List_t(g));List_t(g)
          | _ -> type_error("primapp"))
        | _ -> type_error("primapp"))
    in
    let tc_if (env:tenv) (e1:Mlish_ast.exp) (e2:Mlish_ast.exp) (e3:Mlish_ast.exp):tipe=
      let tc_if_body (env:tenv) (e2:Mlish_ast.exp) (e3:Mlish_ast.exp):tipe=
        match check_guess (tc env e2),check_guess(tc env e3) with
        | Guess_t(t1),Guess_t(t2) -> let g = guess() in t1:=Some g;t2:=Some g;g
        | Guess_t(t1),t2 -> t1:=Some t2;t2
        | t1,Guess_t(t2) -> t2:=Some t1;t1
        | t1,t2 -> if t1=t2 then t1 else type_error("if")
      in
      (match check_guess (tc env e1) with
      | Bool_t -> tc_if_body env e2 e3
      | Guess_t(t) -> t:=Some Bool_t;tc_if_body env e2 e3
      | _ -> type_error("if") )
    in
    let tc_let (env:tenv) (x:Mlish_ast.var) (e1:Mlish_ast.exp) (e2:Mlish_ast.exp):tipe=
      let s = generalize env (tc env e1) in
      print_endline ("generalize "^x^" successfully");
      print_endline (tsci2str s);
      tc (extend env ("var"^x) s) e2
    in
    print_endline (expr2string e);
    print_endline (tenv2str env);
    match e with (e,_) -> match e with
      | Var x -> 
        print_endline ("start instantiate: "^x);
        print_endline (tsci2str (lookup env ("var"^x)));
        let res = instantiate (lookup env ("var"^x)) in
        print_endline (tipe2str res);
        res
      | Fn(x,e) -> 
        let t = guess() in
        let new_env = (extend env ("var"^x) (Forall([],t)) ) in
        Fn_t(t,tc new_env e)
      | App(e1,e2) -> 
        let (t1,t2) = (tc env e1, tc env e2) in 
        let t = guess()
        in 
        (* print_int 99;print_string (tipe2string t1);print_string "applied by";print_string (tipe2string t2); *)
        if unify t1 (Fn_t(t2,t)) then t
        else type_error("app")
      | If(e1,e2,e3) -> tc_if env e1 e2 e3
      | Let(x,e1,e2) -> tc_let env x e1 e2
      | PrimApp(p,e_list) -> tc_PrimApp env p e_list
 
  in 
  (* print_endline (expr2string e); *)
  let res = (tc [] e) in
  print_endline "finish checking";
  res
