module P16 where

dropEvery :: [a] -> Int -> [a]
dropEvery l n = reverse $ helper l [] n
    where
        helper :: [a] -> [a] -> Int -> [a]
        helper [] b _ = b
        helper (x:xs) b 1 = helper xs b n
        helper (x:xs) b c = helper xs (x:b) (c-1)
