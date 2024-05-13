let g = fun x -> 2 * x in
let h = fun x -> fun y -> x + 3 * y in
h (g 10) (g 20)
    
