let id = fun x -> x in
let l = (id :: id :: nil) in
((hd l) true, (hd (tl l)) 3)
