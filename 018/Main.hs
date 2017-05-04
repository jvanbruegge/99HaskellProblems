module P18 where

slice :: [a] -> Int -> Int -> [a]
slice l s e = take (e-s+1) $ drop (s-1) l
