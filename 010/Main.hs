module P10 where

encode :: Eq a => [a] -> [(Int, a)]
encode l =
    let helper [] b c d = (d, b):c
        helper (x:xs) b c d = if x == b then
            helper xs b c (d+1)
        else helper xs x ((d, b):c) 1 in
    reverse $ helper l (head l) [] 0

