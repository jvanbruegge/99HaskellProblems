module P21 where

insertAt :: a -> [a] -> Int -> [a]
insertAt a l n =
    let helper [] e _ = e
        helper d e 1 = helper d (a:e) 0
        helper (x:xs) e b = helper xs (x:e) (b-1) in
    reverse $ helper l [] n
