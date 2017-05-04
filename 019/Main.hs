module P19 where

rotate :: [a] -> Int -> [a]
rotate l n = case n of
    _ | n > 0  -> (drop n l) ++ (take n l)
    _ | n == 0 -> l
    _ | n < 0  -> (drop (length l + n) l) ++ take (length l + n) l
