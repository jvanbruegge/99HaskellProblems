module P15 where

repli :: [a] -> Int -> [a]
repli l n =
    let addb a 0 c = c
        addb a b c = a:(addb a (b-1)) c in
    let helper [] b = b
        helper (x:xs) b = helper xs $ addb x n b in
    reverse $ helper l []
