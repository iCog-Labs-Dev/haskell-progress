module Pangram (isPangram) where
import Data.Char
alphabet :: [Char]
alphabet = ['a' .. 'z'] ++ ['A'..'Z']
produceLower xs = [toLower x | x <- xs, x `elem` alphabet]
removeDuplicates :: [Char] -> [Char]
removeDuplicates [] = []
removeDuplicates (x : xs)
  | x `elem` xs = removeDuplicates xs
  | otherwise = toLower x : removeDuplicates xs
isPangram :: String -> Bool
isPangram text = length (removeDuplicates (produceLower text)) == 26
