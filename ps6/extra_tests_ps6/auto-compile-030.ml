let fold_base = fun f -> (fun l -> (fun init -> init)) in
let fold_rec = fun fold_pre -> fun f -> (fun l -> (fun init ->
 if isnil l then
   fold_pre f l init
 else
   let x1 = hd l in
   let l = tl l in
   f x1 (fold_pre f l init)))
in
let fold1 = fun f l init -> fold_rec fold_base f l init in
let fold2 = fun f l init -> fold_rec fold1 f l init in
let fold3 = fun f l init -> fold_rec fold2 f l init in
let fold4 = fun f l init -> fold_rec fold3 f l init in
let fold5 = fun f l init -> fold_rec fold4 f l init in
let plus = fun x -> fun acc -> x + acc in
let numtrue = fun x -> fun acc -> (if x then 1 else 0) + acc in
fold5 plus (1 :: 2 :: 3 :: 4 :: 5 :: nil) 0 +
fold5 numtrue (true :: false :: false :: false :: true :: nil) 0
