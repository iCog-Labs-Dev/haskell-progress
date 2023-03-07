module Anagram (anagramsFor) where
import Data.Char (toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs = filter (isAnagram xs)

isAnagram :: String -> String -> Bool
isAnagram xs ys = let xsl = map toLower xs; ysl = map toLower ys
                  in sort xsl == sort ysl && xsl /= ysl