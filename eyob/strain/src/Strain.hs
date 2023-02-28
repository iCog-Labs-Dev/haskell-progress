module Strain (keep, discard) where

discard :: (a -> Bool) -> [a] -> [a]
discard _ [] = []
discard p xs =
  if not . p $ head xs
    then head xs : discard p (tail xs)
    else discard p (tail xs)

keep :: (a -> Bool) -> [a] -> [a]
keep _ [] = []
keep p xs =
  if p $ head xs
    then head xs : keep p (tail xs)
    else keep p (tail xs)
