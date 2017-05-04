module P11 where

data RunlengthTuple a = Multiple Int a | Single a deriving Show

encodeModified :: Eq a =>[a] -> [RunlengthTuple a]
encodeModified l =
    let getTuple b d = if d == 1 then Single b else Multiple d b in
    let helper [] b c d = (getTuple b d):c
        helper (x:xs) b c d = if x == b then
            helper xs b c (d+1) else
            helper xs x ((getTuple b d):c) 1 in
    reverse $ helper l (head l) [] 0
