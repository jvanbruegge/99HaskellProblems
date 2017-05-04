module P20 where

removeAt :: Int -> [a] -> (a, [a])
removeAt n l =
    let helper [] _ b = b
        helper (x:xs) 1 (_, b) = helper xs 0 (x, b)
        helper (x:xs) a (c, b) = helper xs (a-1) (c, (x:b)) in
    let (a, b) = helper l n (head l, []) in
    (a, reverse b)
