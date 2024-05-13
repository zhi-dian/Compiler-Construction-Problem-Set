let map2 = fun f -> (fun l ->
 if isnil l then
   nil
 else
   let x1 = hd l in
   let l = tl l in
    if isnil l then
      (f x1) :: nil
    else
      let x2 = hd l in
      (f x1) :: (f x2) :: nil) in
let id = fun x -> x in
(map2 id (1 :: 2 :: 3 :: nil),
 map2 id (true :: nil))
    
