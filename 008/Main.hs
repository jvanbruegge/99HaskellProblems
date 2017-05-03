module P8 where

compress :: Eq a => [a] -> [a]
compress l =
    let helper [] b = b
        helper (x:xs) [] = helper xs [x]
        helper (x:xs) b = helper xs (if x == head b then b else x:b) in
    reverse $ helper l []
