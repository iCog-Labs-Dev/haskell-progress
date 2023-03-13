module Luhn (isValid) where
import Data.Char(isSpace)
isValid :: String -> Bool
isValid n 
       | length (filter (not . isSpace) n ) <= 1 = False
       | otherwise = if (filterNumber (doubleNumber (toDigits (toInt (reverse n))))) `mod` 10 == 0 then True else False

toInt :: String -> Integer
toInt str = read ([x | x <- str, (not . isSpace) x]) :: Integer

toDigits :: Integer -> [Integer]
toDigits 0  = []
toDigits n 
        | n < 0 = []
        | n < 10 = [n]
        | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]


doubleNumber :: [Integer] -> [Integer]
doubleNumber n =  map (\(i, x) -> if odd i then x * 2 else x) (zip [0..] n)
             

filterNumber :: [Integer] -> Integer
filterNumber n = sum([if x > 9 then x-9 else x | x <- n])