module P9 where

pack :: Eq a => [a] -> [[a]]
pack l =
    let helper [] b c = b:c
        helper (x:xs) [] c = helper xs [x] c
        helper (x:xs) b c = if x == head b then
            helper xs (x:b) c else
            helper xs [x] (b:c) in
    reverse $ helper l [] []
