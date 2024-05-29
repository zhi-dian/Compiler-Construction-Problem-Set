let max_pair = fun p -> if (fst p) < (snd p) then snd p else fst p in
max_pair (max_pair(1, 2), max_pair (8, 9))
