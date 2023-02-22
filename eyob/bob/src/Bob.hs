module Bob (responseFor) where

import Data.Char (isAlpha, isLetter, isSpace, isUpper)

trim :: String -> String
trim = filter (not . isSpace)

responseFor :: String -> String
-- responseFor xs = error "You need to implement this function."
responseFor question
  | question == "" || addressSilently = "Fine. Be that way!"
  | isAllUpperCase && endsWithQuestionMark = "Calm down, I know what I'm doing!"
  | endsWithQuestionMark = "Sure."
  | isAllUpperCase = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    endsWithQuestionMark = last (trim question) == '?'
    onlyWords = filter isAlpha question
    isAllUpperCase = all isUpper onlyWords && onlyWords /= ""
    addressSilently = all isSpace question

-- This is an alternative solution
-- responseFor input
--     | null text = "Fine. Be that way!"
--     | isShouting && isAsking = "Calm down, I know what I'm doing!"
--     | isShouting = "Whoa, chill out!"
--     | isAsking = "Sure."
--     | otherwise = "Whatever."
--   where
--     text = filter (not . isSpace) input
--     letters = filter isLetter text
--     isShouting = all isUpper letters && any isUpper letters
--     isAsking = last text == '?'