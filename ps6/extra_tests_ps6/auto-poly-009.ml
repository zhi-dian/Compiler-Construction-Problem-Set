let fold2 = fun f -> (fun l -> (fun init ->
 if isnil l then
   init
 else
   let x1 = hd l in
   let l = tl l in
    if isnil l then
      f x1 init
    else
      let x2 = hd l in
      f x2 (f x1 init))) in
let recons = fun x -> fun acc -> x :: acc in
(fold2 recons (1 :: 2 :: 3 :: nil) nil,
 fold2 recons (true :: false :: true :: nil) nil)
