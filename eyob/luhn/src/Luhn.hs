module Luhn (isValid) where

import Data.Char (isDigit, isSpace)

stripSpaces :: String -> String
stripSpaces = filter (not . isSpace)

hasOnlyDigits :: String -> Bool
hasOnlyDigits = all isDigit

isStringGRTTwo :: String -> Bool
isStringGRTTwo = (> 1) . length

doubleEven :: [Int] -> [Int]
doubleEven [] = []
doubleEven [x] = [x]
doubleEven (x : y : xs) = x : calcDouble : doubleEven xs
  where
    calcDouble = if y * 2 > 9 then y * 2 - 9 else y * 2

isValid :: String -> Bool
isValid n
  | isStringGRTTwo n' && hasOnlyDigits n' = sum (doubleEven (map (\x -> read [x] :: Int) $ reverse n')) `mod` 10 == 0
  | otherwise = False
  where
    n' = stripSpaces n