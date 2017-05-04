module P14 where

dupli :: [a] -> [a]
dupli l =
    let helper [] b = b
        helper (x:xs) b = helper xs (x:x:b) in
    reverse $ helper l []
