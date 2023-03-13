-- https://exercism.org/tracks/haskell/exercises/luhn/

module Luhn (isValid) where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
    | n <= 0 = []
    | otherwise = n `mod` 10 : toDigitsRev (floor (fromIntegral n / 10))


toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


doubleEveryOtherHelper :: [Integer] -> [Integer]
doubleEveryOtherHelper [] = []
doubleEveryOtherHelper (x:y:xs) = x * 2 : y : doubleEveryOtherHelper xs


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
    | even (length xs) = doubleEveryOtherHelper xs
    | otherwise = head xs : doubleEveryOtherHelper (tail xs)


sumDigits :: [Integer] -> Integer
sumDigits = sum


isValid :: Integer -> Bool
isValid n = sumDigits (map (sumDigits . toDigits) $ doubleEveryOther $ toDigits n) `mod` 10 == 0