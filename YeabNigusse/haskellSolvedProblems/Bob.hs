module Bob (responseFor) where
import Data.Char (isSpace, toUpper, isAlpha)
import Data.List (all)
responseFor :: String -> String
responseFor xs 
         | questionAtHim xs  = "Calm down, I know what I'm doing!"
         | toSure xs = "Sure."
         | toChill xs  =  "Whoa, chill out!" 
         | toFine xs = "Fine. Be that way!"
         | otherwise = "Whatever."

questionAtHim :: String -> Bool
questionAtHim "" = False
questionAtHim "          " = False
questionAtHim "          " = False
questionAtHim "\n\r \t" = False
questionAtHim s = last s == '?' && s == map toUpper s &&  isAlpha(last (init s)) 

toSure :: String -> Bool
toSure "" = False
toSure "          " = False
toSure "\t\t\t\t\t\t\t\t\t\t" = False
toSure "\n\r \t" = False
toSure x = last (filter (not . isSpace) x) == '?' 

toChill :: String -> Bool
toChill "" = False
toChill "          " = False
toChill "\t\t\t\t\t\t\t\t\t\t" = False
toChill "\n\r \t" = False
toChill x = x == map toUpper x && last x /= '3' && not (all isSpace x )

toFine :: String -> Bool
toFine "" = True
toFine x = '\t' `elem` x || isWhitespace x 

isWhitespace :: String -> Bool
isWhitespace str = all isSpace str

