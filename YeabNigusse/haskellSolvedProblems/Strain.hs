module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard p xs = filter (not .p) xs

keep :: (a -> Bool) -> [a] -> [a]
keep p xs = filter p xs


