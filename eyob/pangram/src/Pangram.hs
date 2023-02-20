module Pangram (isPangram) where

import Data.Char (toLower)

isPangram :: String -> Bool
-- First implementation
-- isPangram text = all (`elem` map toLower text) ['a' .. 'z']

-- Another custom implementation
isPangram text =
  all (checkForLetter normalizedText) ['a' .. 'z']
  where
    normalizedText = map toLower text
    checkForLetter [] _ = False
    checkForLetter (y : ys) c
      | c == y = True
      | otherwise = checkForLetter ys c