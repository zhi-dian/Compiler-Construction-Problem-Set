let h = fun x -> 3 in
    h 1 :: h true :: h () :: nil
