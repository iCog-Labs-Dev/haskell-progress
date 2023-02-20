module Strain (keep, discard) where

produceList :: (a -> Bool) -> [a] -> [a]
produceList _ [] = []
produceList p (x:xs) | p x = x : keep p xs
          | otherwise = keep p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = produceList p xs

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = produceList (not . p) xs
