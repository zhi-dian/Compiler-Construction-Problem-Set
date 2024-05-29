let swap = fun ab -> (snd ab, fst ab) in
let pairid = fun x -> (swap (swap x)) in
(swap (1, true), pairid (true, 1))
