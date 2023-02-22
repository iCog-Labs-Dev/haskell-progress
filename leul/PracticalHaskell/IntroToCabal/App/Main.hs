module PracticalHaskell.IntroToCabal.App.Main where

-- check if first element is empty or if the list is empty

isEmpty lst = null lst || null (head lst)

hasOnlyOneElement :: [a] -> Bool
hasOnlyOneElement lst = not (null lst) && null (tail lst)

concatenate :: [[a]] -> [a]
concatenate lst = head lst ++ last lst

concatRecursive :: [[a]] -> [a]
concatRecursive lst = if null lst then [] else head lst ++ concatRecursive (tail lst)

reverse2 :: [a] -> [a]
reverse2 lst =
  if null lst
    then lst
    else last lst : reverse2 (init lst)
