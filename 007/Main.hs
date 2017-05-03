module P7 where

data NestedList a = Elem a | List [NestedList a]

flatten :: NestedList a -> [a]
flatten l =
    let helper (List []) b = b
        helper (Elem a) b = a:b
        helper (List (x:xs)) b = helper (List xs) (helper x b) in
    reverse $ helper l []
