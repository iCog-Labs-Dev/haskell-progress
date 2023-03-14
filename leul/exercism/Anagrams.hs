
-- https://exercism.org/tracks/haskell/exercises/anagram/

module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagramsFor :: String -> [String] -> [String]
anagramsFor xs xss = filter ((==sortedInput) . sort . map toLower) $ 
                        filter ((/=lowerCasedWord) . map toLower) xss
    where sortedInput = sort lowerCasedWord
          lowerCasedWord = map toLower xs          

