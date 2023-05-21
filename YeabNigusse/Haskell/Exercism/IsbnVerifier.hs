module IsbnVerifier (isbn) where
import Data.Char
isbn :: String -> Bool
isbn str 
       | str == "" = False
       | length (if last str == 'X' then convert str ++ [10] else convert str) /= 10 = False
       | otherwise = check (if last str == 'X' then convert str ++ [10] else convert str)

convert :: String -> [Integer]
convert "" = []
convert (x:xs)
        | x == '0' = 0 : convert xs
        | x == '1' = 1 : convert xs
        | x == '2' = 2 : convert xs
        | x == '3' = 3 : convert xs
        | x == '4' = 4 : convert xs
        | x == '5' = 5 : convert xs
        | x == '6' = 6 : convert xs
        | x == '7' = 7 : convert xs
        | x == '8' = 8 : convert xs
        | x == '9' = 9 : convert xs
        | otherwise = convert xs

check :: Integral a => [a] -> Bool
check  lst = sum(zipWith (*) [10,9 ..1] lst) `mod` 11 == 0
