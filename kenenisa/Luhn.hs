module Luhn (isValid) where

import Data.Char

doubleOdds :: Int -> [Int] -> Int
doubleOdds acc [] = 0
doubleOdds acc [x] = acc + xDoubled x
doubleOdds acc (x : y : xs) = acc + x + xDoubled y + doubleOdds acc xs

xDoubled :: Int -> Int
xDoubled x
  | x * 2 > 9 = (x * 2) - 9
  | otherwise = x * 2

isValid :: [Char] -> Bool
isValid n
  | length filtered < 2 = False
  | otherwise = doubleOdds 0 (reverse $ map (\x -> (read [x] :: Int)) filtered) `mod` 10 == 0
  where
    filtered = filter isDigit n
