module Acronym (abbreviate) where

import Data.Char (isLetter, isSpace, isUpper, toLower, toUpper)
import Data.List (partition)

stripStart :: String -> String
stripStart = dropWhile isSpace

separateWords :: String -> [String]
separateWords [] = []
separateWords sentence = takeWhile isNotEqualToSpaceAndDash checkedSentence : separateWords (stripStart (dropWhile isNotEqualToSpaceAndDash checkedSentence))
  where
    checkedSentence = if isLetter (head sentence) then sentence else tail sentence
    isNotEqualToSpaceAndDash x = x /= ' ' && x /= '-'

capitalize :: String -> String
capitalize [] = []
capitalize sentence = unwords (map capitalizeWord (separateWords (stripStart sentence)))
  where
    capitalizeWord [] = []
    capitalizeWord word@(x : xs) =
      if all isUpper word
        then toUpper x : map toLower xs
        else toUpper x : xs

abbreviate :: String -> String
abbreviate [] = []
abbreviate sentence = fst (partition (`elem` ['A' .. 'Z']) (capitalize sentence))