module P6 where

isPalindrome :: Eq a => [a] -> Bool
isPalindrome l = l == reverse l
