let f = fun x -> fun y -> x + x + y in
let g = f 3 in
g 1 + g 2 
