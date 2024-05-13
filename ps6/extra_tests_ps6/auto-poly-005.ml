let comp = fun f -> fun g -> fun x -> f (g x) in
let incr = fun x -> x + 1 in
let double = fun x -> x * 2 in
let negate = fun x -> if x then true else false in
(comp incr double 4, comp negate (fun x -> x < 3) 3)
