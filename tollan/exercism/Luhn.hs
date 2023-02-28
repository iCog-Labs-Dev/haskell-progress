module Luhn (isValid) where

isValid :: String -> Bool
isValid n = if (validFormat) then (checkLuhn noSpaces 0 []) else False
    where noSpaces = filter (/=' ') n
          allNumbers = all (`elem` ['0'..'9']) noSpaces
          validLength = length noSpaces > 1
          validFormat = allNumbers && validLength

checkLuhn :: String -> Int -> [Int] -> Bool
checkLuhn num idx acc
    | idx == length num = (sum acc) `mod` 10 == 0
    | curParity idx = checkLuhn num (idx+1) (acc ++ [doubleCase])
    | otherwise = checkLuhn num (idx+1) (acc ++ [curNum])
    where curNum = read [num !! idx] :: Int
          curParity = if (even $ length num) then even else odd
          doubleCase = if (2*curNum) > 9 then (2*curNum - 9) else (2*curNum)