module Anagram (anagramsFor) where
import Data.Char
import Data.List

anagramsFor :: String -> [String] -> [String]
anagramsFor [] _ = []
anagramsFor _ [] = []
anagramsFor xs xss = checkAnagram xs xss []

checkAnagram :: String -> [String] -> [String] -> [String]
checkAnagram string [] winners = winners
checkAnagram string (can:candidates) winners = 
  if ((sort $ map toUpper string) == (sort $ map toUpper can)) && (map toUpper string /= map toUpper can)
  then checkAnagram string candidates (winners++[can])
  else checkAnagram string candidates winners
