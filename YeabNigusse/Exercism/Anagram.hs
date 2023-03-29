module Anagram (anagramsFor) where
import Data.Char(toLower)
import Data.List (sort)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = [ y | y <- xss, sortString xs == sortString y,map toLower xs /= map toLower y]


sortString :: String -> String
sortString = sort . map toLower



