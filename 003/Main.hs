module P3 where

elementAt :: [a] -> Int -> a
elementAt (x:xs) 1 = x
elementAt (x:xs) n = elementAt xs (n-1)
