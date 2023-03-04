module Acronym (abbreviate) where
import Data.Maybe (maybeToList)
import Data.Char (isAlpha, isSpace, toUpper)

abbreviate :: String -> String
abbreviate xs = ""

alphaOnly :: String -> String
alphaOnly = map (\c -> if isAlpha c || isSpace c then c else ' ')

toList :: String -> [String]
toList  = words

toString :: [String] -> String
toString = unwords

--filterAcro :: String -> String
--filterAcro  = foldl (\accu x -> accu ++ take 1 x) "" . toString (toList)

myAcro :: String -> [a]
myAcro xs = [c | c <- xs, c == ' ' ++ c] -- filtering characters by checking whether a character come after space or not
