module CryptoSquare (encode) where
import Data.List (transpose, intercalate, union)
import Data.Char (toLower)

encode :: String -> String
encode [] = []
encode x = encoded
    where xs = normalize x
          lengthXs = length xs
          (row, col) = rowCol lengthXs [1..]
          text = fillWithSpaces (row*col - lengthXs) xs
          matrix = produceMatrix text col
          encoded = intercalate " " (transpose matrix)

normalize :: String -> String
normalize str = let strLower = map toLower str in [x | x <- strLower, x `elem` (['a'..'z'] ++ ['0'..'9'])]

rowCol :: Int -> [Int] -> (Int, Int)
rowCol len (a:b:rest) 
    | a*a >= len = (a,a)
    | a*b >= len = (a,b)
    | otherwise = rowCol len (b:rest)

fillWithSpaces :: Int -> String -> String
fillWithSpaces 0 str = str
fillWithSpaces len str = fillWithSpaces (len-1) (str ++ " ")

produceMatrix :: String -> Int -> [String]
produceMatrix [] _ = []
produceMatrix str col = [row] ++ produceMatrix rest col
    where (row, rest) = splitAt col str
