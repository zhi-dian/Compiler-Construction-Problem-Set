let fold2 = fun f -> (fun l -> (fun init ->
    let x1 = hd l in
    let b = tl l in
       let x2 = hd b in
       f x2 (f x1 init))) in
 fold2 (fun x -> (fun acc -> x + acc)) (1 :: 2 :: 3 :: nil) 0
 