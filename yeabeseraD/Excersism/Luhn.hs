module Luhn (isValid) where
import Data.Char (isSpace, isLetter)

isValid :: String -> Bool
isValid n
    | length (filtereN n) < 2 = False
    | not (all isLetter n ) = False
    | otherwise = validate ( filtereN $ unwords changedN)
    where filtereN = filter (not . isSpace)
          changedN = map convert (words n)

convert :: String -> String
convert (a:x:b:y)
        | read [x] > 9 = convert (a: head (show (read [x] - 9)) : b : y)
        | read y > 9 = convert (a: x : b : show (read y - 9))
        | otherwise = convert(a: head (show (read [x] * 2)):b: show (read y * 2))

validate :: String -> Bool
validate xs = sum (map (\x -> read [x]) xs) `mod` 10 == 0