module P12 where

data RunlengthTuple a = Single a | Multiple Int a deriving Show

decodeModified :: [RunlengthTuple a] -> [a]
decodeModified l =
    let helper [] b = b
        helper ((Single a):xs) b = helper xs (a:b)
        helper ((Multiple 0 _):xs) b = helper xs b
        helper ((Multiple d a):xs) b = helper ((Multiple (d-1) a):xs) (a:b) in
    reverse $ helper l []
