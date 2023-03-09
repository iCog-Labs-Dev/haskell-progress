import Data.List
import Data.Char (digitToInt)

toDigits :: Integer -> [Integer]
toDigits digit = map (toInteger . digitToInt) (show digit)

toDigitsRev :: Integer -> [Integer]
toDigitsRev = reverse . toDigits

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther = reverse . doubleDigits . reverse
        where doubleDigits [] = []
              doubleDigits [x] = [x]
              doubleDigits (x:y:xs) = x : (y*2) : doubleDigits xs

sumDigits :: [Integer] -> Integer
sumDigits = sum . concatMap toDigits 

validate :: Integer -> Bool
validate x = sumDigits (doubleEveryOther $ toDigits x) `mod` 10 == 0