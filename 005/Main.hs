module P5 where

myReverse :: [a] -> [a]
myReverse [] = []
myReverse l =
    let helper [] b = b
        helper (x:xs) b = helper xs (x:b) in
    helper l []
