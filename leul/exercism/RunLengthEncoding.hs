-- https://exercism.org/tracks/haskell/exercises/run-length-encoding/

module RunLength (decode, encode) where

isDigit :: Char -> Bool
isDigit x = x `elem` ['0'..'9']


countConsecutive :: String -> (Int, Char, String)
countConsecutive [] = (0, '\0', "")
countConsecutive (x:xs) = (count + 1, x, rest)
    where count = length $ takeWhile (==x) xs
          rest  = drop count xs
        

expandFront :: String -> (Int, Char, String)
expandFront s = (read count, char, rest)
    where count = takeWhile isDigit s
          digitLength = (length count) 
          char = s !! digitLength
          rest = drop (digitLength + 1) s


decode :: String -> String
decode [] = []
decode encodedText@(x:xs)
    | not (isDigit x) = char : decode xs
    | otherwise = (replicate count char) ++ decode rest
    where (count, char, rest) = expandFront encodedText

encode :: String -> String
encode [] = []
encode text
    | count > 1 = (show count) ++ char : encode rest
    | otherwise = char : encode rest
    where (count, char, rest) = countConsecutive text
    
