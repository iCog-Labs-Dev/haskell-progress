-- https://www.seas.upenn.edu/~cis1940/spring13/hw/01-intro.pdf

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