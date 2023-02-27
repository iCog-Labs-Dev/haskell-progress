module Anagram (anagramsFor) where

import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor _ [] = []
anagramsFor xs (x : xss)
  | normalize xs == normalize x = anagramsFor xs xss
  | sort (normalize xs) == sort (normalize x) = x : anagramsFor xs xss
  | otherwise = anagramsFor xs xss
  where
    normalize = map toLower