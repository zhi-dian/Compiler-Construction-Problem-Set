let thrd = fun x -> snd (snd x) in
(thrd (true, (1, (3 :: nil))),
 thrd (3 :: nil, (1, (true))))
