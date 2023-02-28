module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = [a | a<-xs , not $ p a]

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = [a | a<-xs , p a]
