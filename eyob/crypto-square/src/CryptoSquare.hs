module CryptoSquare (encode) where

import Data.Char (isAlphaNum, isSpace, toLower)
import Data.List (transpose, unwords)

-- Remove spaces and punctuation
getNormalized :: String -> String
getNormalized xs = map toLower $ filter (\x -> (not . isSpace) x && isAlphaNum x) xs

-- Get the length of the normalized string
getNormalizedLength :: String -> Int
getNormalizedLength xs = length $ getNormalized xs

-- Get the factors of a number
factors :: Int -> [Int]
factors n = [x | x <- [1 .. n], n `mod` x == 0]

-- Check if the middle two factors satisfy the condition if not,
-- add 1 to the first factor and subtract 1 from the second factor
-- and repeat until the condition is satisfied
recursiveCheck :: Int -> Int -> Int
recursiveCheck r c =
  if c >= r && c - r <= 1
    then c
    else recursiveCheck (r + 1) (c - 1)

-- Get the number of columns from the normalized string
getNoOfCols :: String -> Int
getNoOfCols xs =
  if odd normalLen
    then c
    else recursiveCheck r c
  where
    normalLen = getNormalizedLength xs
    normalFactor = factors normalLen
    normalFactorLen = length normalFactor
    divideByTwo = normalFactorLen `div` 2
    r = normalFactor !! pred (if divideByTwo == 0 then normalFactorLen else divideByTwo)
    c = normalFactor !! divideByTwo

-- Get the number of rows from the normalized string and the number of columns
getNoOfRows :: String -> Int -> Int
getNoOfRows xs c = ceiling $ fromIntegral (getNormalizedLength xs) / fromIntegral c

-- Get the rows from the normalized string and the number of columns
getRows :: String -> [String]
getRows xs = map (\x -> take c $ drop (x * c) normalized) [0 .. r - 1]
  where
    normalized = getNormalized xs
    c = getNoOfCols xs
    r = getNoOfRows xs c

-- Normalize the rows to the same length
normalizeRow :: String -> Int -> String
normalizeRow xs c = take c $ xs ++ repeat ' '

-- Normalize the rows
normalizeRows :: [String] -> [String]
normalizeRows xs = map (\row -> normalizeRow row m) $ xs
  where
    m = maximum $ map length xs

-- Encode the string using the Crypto Square algorithm
encode :: String -> String
encode xs
  | null xs = []
  | otherwise = unwords $ transpose $ normalizeRows $ getRows xs
