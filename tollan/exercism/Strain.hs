module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = [] 
discard p (x:xs)
    | p x = discard p xs
    | otherwise = x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [x | x <- xs, p x]
