let rotate = fun x -> (fst (snd x), ((snd (snd x)), fst x)) in
let x = (0, (true, ())) in
(rotate x, (rotate (rotate x), rotate (rotate (rotate x))))
                       
