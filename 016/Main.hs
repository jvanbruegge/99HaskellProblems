module P16 where

dropEvery :: [a] -> Int -> [a]
dropEvery l n =
    let helper [] b _ = b
        helper (x:xs) b 1 = helper xs b n
        helper (x:xs) b c = helper xs (x:b) (c-1) in
    reverse $ helper l [] n
