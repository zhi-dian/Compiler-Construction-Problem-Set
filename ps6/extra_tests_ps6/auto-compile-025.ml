let eval = fun p -> (fst p) (snd p) in
eval (fun x -> x + 1, 26)
