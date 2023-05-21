module Acronym (abbreviate) where
import Data.Char (isAlpha, isSpace, toUpper, isUpper, isLower)

abbreviate :: String -> String
abbreviate xs = filter (not.isSpace)(alphaOnly (myAcro xs))

alphaOnly :: String -> String
alphaOnly = map (\c -> if isAlpha c || isSpace c || c == '\''then c else ' ')

myAcro :: String -> String
myAcro = map toUpper . myAcro' . alphaOnly
    where myAcro' xs = [c | (c, prev) <- zip xs (' ':xs), prev == ' ' || isUpper c && isLower prev]
