-- https://exercism.org/tracks/haskell/exercises/isogram

module Isogram (isIsogram) where

import Data.Char (toLower)
import Data.List (nub)


letters :: String
letters = ['a'..'z'] ++ ['A'..'Z']


clean :: String -> String
clean str = [s | s <- str, s `elem` letters]


isIsogram :: String -> Bool
isIsogram str = length (nub cleaned) == length cleaned
    where cleaned = map toLower $ clean str
