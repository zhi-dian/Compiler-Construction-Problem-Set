let double = fun f -> (fun x -> f (f x)) in
let incr = fun x -> x + 1 in
let negate = fun x -> if x then false else true in
(double incr 1, double negate true)
       
