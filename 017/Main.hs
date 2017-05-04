module P17 where

split :: [a] -> Int -> ([a], [a])
split l n = (take n l, drop n l)
