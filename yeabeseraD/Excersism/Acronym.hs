module Acronym (abbreviate) where
import Data.Char (toUpper, toLower, isUpper, isAlpha)
import Data.List (words, concat)

abbreviate :: String -> String
abbreviate xs = filter isUpper concatinatedStrings
    where xs' = map (\y -> if isAlpha y || y == '\'' then y else ' ') xs
          listOfStrings = words xs'
          filteredStrings = map (\(x:xs) -> if all isUpper (x:xs) then x: map toLower xs else x:xs) listOfStrings
          upperCaseStrings = map (\(x:xs) -> toUpper x : xs ) filteredStrings
          concatinatedStrings = concat upperCaseStrings