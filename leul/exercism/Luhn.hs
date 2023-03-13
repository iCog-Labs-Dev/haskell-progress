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


validate :: Integer -> Bool
validate n = sumDigits (map (sumDigits . toDigits) $ doubleEveryOther $ toDigits n) `mod` 10 == 0


convert :: Integer -> String -> Integer
convert _ [] = 0
convert n (' ':xs) = convert n xs
convert n (x:xs) = (read [x] :: Integer) * (10 ^ n) + convert (n + 1) xs


trim :: String -> String
trim [] = []
trim x = reverse $ dropWhile (==' ') $ reverse $ dropWhile (==' ') x


isValid :: String -> Bool
isValid card
    | trim card == "0" = False
    | otherwise = (validate . convert 0 . reverse . trim) card