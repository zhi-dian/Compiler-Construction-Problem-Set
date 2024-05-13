let eval = fun fx -> (fst fx) (snd fx) in
(eval (fun x -> x + 1, 3), eval (fun x -> isnil x, 3 :: nil))
