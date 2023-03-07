module Luhn (isValid) where
import Data.Char (isSpace, isNumber, isDigit, digitToInt)

isValid :: String -> Bool
isValid n 
    | hasValidLength n' && isAllDigit n' = sum n'' `mod` 10 == 0
    | otherwise = False
    where n' = stripSpaces n
          n'' =convertDigit $ reverse $ toDigit n'

hasValidLength :: String -> Bool
hasValidLength a = length a > 1

stripSpaces :: String -> String
stripSpaces = filter (not . isSpace)

isAllDigit :: String -> Bool
isAllDigit = all isDigit

toDigit :: String -> [Int]
toDigit = map digitToInt

convertDigit :: [Int] -> [Int]
convertDigit [] = []
convertDigit [x] = [x]
convertDigit (x:y:xs) = x : double y : convertDigit xs
    where double x = if x * 2 > 9
                        then x * 2 - 9
                        else x * 2