open Mlish_ast

exception TypeError
let type_error(s:string) = (print_string s; raise TypeError)


module StringtoType = struct
  type t = (var*Mlish_ast.tipe) list
end

let extend (env:StringtoType.t) (x:Mlish_ast.var) (t:Mlish_ast.tipe): StringtoType.t= 
  [(x,t)]@env

let rec lookup (env:StringtoType.t) (x:Mlish_ast.var): Mlish_ast.tipe=
  match env with
  | [] -> type_error("")
  | (xa,ta)::res -> if xa = x then ta else (lookup res x)

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
  
let type_check_exp (e:Mlish_ast.exp) : tipe =
  let rec tc (env:StringtoType.t) (e:Mlish_ast.exp) : tipe= 
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
      (* (print_endline ("unify"^(tipe2str t1)^"and"^(tipe2str t2))); *)
      if (is_equal t1 t2) then (true) else
      (match t1,t2 with
      | Guess_t(t1'), _ -> 
        (match !t1' with 
        (* | None -> t1':= Some t2;true *)
        | None -> (*avoid something like Guess(t1), t1=Fn(t1)*)
          (match t2 with
            | Guess_t(t2') -> if t2'==t1' then true else 
              (match !t2' with
              | None -> if t2'==t1' then true else (t1':=Some t2;true)
              | Some t2'' -> unify t2 t1)
            | _ -> if occurs t1' t2 then true else (t1':=Some t2;true))
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
      | _,_ -> type_error(""))
    in
    (* let rec check_guess_list (t1:Mlish_ast.tipe): tipe = (*return normal type or empty guess*)
      match t1 with
      | Guess_t(t1') -> 
        (match !t1' with 
        | Some t1' -> check_guess_list t1'
        | None -> t1)
      | List_t(t1') -> List_t(check_guess_list t1')
      | ti -> ti
    in *)
    let rec tc_PrimApp (env:StringtoType.t)(p:Mlish_ast.prim) (e_list:Mlish_ast.exp list): tipe = 
      let check_binary_int (env:StringtoType.t)(e_list:Mlish_ast.exp list)(return_tipe:tipe): tipe = 
        match e_list with 
        | e1::e2::[] -> 
          (match (check_guess (tc env e1),check_guess (tc env e2)) with
          | Int_t,Int_t -> return_tipe
          | Guess_t(t1),Guess_t(t2) -> t1:=Some Int_t;t2:=Some Int_t;return_tipe
          | Guess_t(t1),Int_t -> t1:=Some Int_t;return_tipe
          | Int_t,Guess_t(t2) -> t2:=Some Int_t;return_tipe
          | _ -> type_error(""))
          (* if check_guess env (tc env e1) (tc env e2) then return_tipe else type_error("") *)
        | _ -> type_error("")
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
          | _ -> type_error("")) in 
          (* print_string (tipe2string res); *)
          res
      | Fst ->
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          (* (match tc env e1 with *)
          | Pair_t(t1,_) -> t1
          | Guess_t(t) -> let g1 = guess() in let g2 = guess() in t:= Some (Pair_t(g1,g2));g1
          | _ -> type_error(""))
        | _ -> type_error(""))
      | Snd ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          (* (match tc env e1 with *)
          | Pair_t(t1,_) -> t1
          | Guess_t(t) -> let g1 = guess() in let g2 = guess() in t:= Some (Pair_t(g1,g2));g2
          | _ -> type_error(""))
        | _ -> type_error(""))
      | Nil -> let g = guess() in if e_list=[] then List_t(g) else type_error("")
      | Cons ->   
        (match e_list with 
        | e1::e2::[] -> 
          (match (check_guess (tc env e1),check_guess (tc env e2)) with
          | Guess_t(t1),Guess_t(t2) -> let g = guess() in t1:=Some g;t2:=Some (List_t(g));List_t(g)
          (*how does list guarantee t1, t2 has same type if they are all none?*)
          | Guess_t(t1),t2 -> 
              (match t2 with
              |List_t(t2') -> t1:=Some t2';t2
              | _ -> type_error(""))
          | t1,Guess_t(t2) -> t2:=Some (List_t(t1));List_t(t1)
          | t1,List_t(t2) -> (match (check_guess t2) with
            | Guess_t(t2') -> t2':= Some t1;List_t(t1)
            | t2' -> if t1=t2' then List_t(t1) else type_error(""))
          | _,_ -> type_error(""))
        | _ -> type_error(""))
      | IsNil ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(_) -> Bool_t
          | Guess_t(t) -> let g1 = guess() in t:= Some (List_t(g1));Bool_t
          | _ -> type_error(""))
        | _ -> type_error(""))
      | Hd ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(t) -> t
          | Guess_t(t) -> let g = guess() in t:=Some (List_t(g));g
          | _ -> type_error(""))
        | _ -> type_error(""))
      | Tl ->   
        (match e_list with 
        | e1::[] -> 
          (match check_guess (tc env e1) with
          | List_t(t) -> List_t(t)
          | Guess_t(t) -> let g = guess() in t:=Some (List_t(g));List_t(g)
          | _ -> type_error(""))
        | _ -> type_error(""))
    in
    let tc_if (env:StringtoType.t) (e1:Mlish_ast.exp) (e2:Mlish_ast.exp) (e3:Mlish_ast.exp):tipe=
      let tc_if_body (env:StringtoType.t) (e2:Mlish_ast.exp) (e3:Mlish_ast.exp):tipe=
        match check_guess (tc env e2),check_guess(tc env e3) with
        | Guess_t(t1),Guess_t(t2) -> let g = guess() in t1:=Some g;t2:=Some g;g
        | Guess_t(t1),t2 -> t1:=Some t2;t2
        | t1,Guess_t(t2) -> t2:=Some t1;t1
        | t1,t2 -> if t1=t2 then t1 else type_error("")
      in
      (match check_guess (tc env e1) with
      | Bool_t -> tc_if_body env e2 e3
      | Guess_t(t) -> t:=Some Bool_t;tc_if_body env e2 e3
      | _ -> type_error("") )
    in
    let tc_let (env:StringtoType.t) (x:Mlish_ast.var) (e1:Mlish_ast.exp) (e2:Mlish_ast.exp):tipe=
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

      in
      let upd_e2 = 
        (substitue e1 x e2)
      in
      (* (print_endline (expr2string upd_e2)); *)
      (tc env upd_e2)
    in
    match e with (e,_) -> match e with
      | Var x -> lookup env ("var"^x)
      | Fn(x,e) -> 
        let t = guess() in
        Fn_t(t,tc (extend env ("var"^x) t) e)
      | App(e1,e2) -> 
        let (t1,t2) = (tc env e1, tc env e2) in 
        let t = guess()
        in 
        (* print_int 99;print_string (tipe2string t1);print_string "applied by";print_string (tipe2string t2); *)
        if unify t1 (Fn_t(t2,t)) then t
        else type_error("")
      | If(e1,e2,e3) -> tc_if env e1 e2 e3
      | Let(x,e1,e2) -> tc_let env x e1 e2
      | PrimApp(p,e_list) -> tc_PrimApp env p e_list
 
  in 
  (* print_endline (expr2string e); *)
  (tc [] e)
