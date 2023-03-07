-- https://exercism.org/tracks/haskell/exercises/crypto-square/

module CryptoSquare (encode) where

import Data.Char (toLower)

allowed :: [Char]
allowed = ['A'..'Z'] ++ ['a'..'z'] ++ ['0'..'9']

normalize :: String -> String
normalize str = map toLower $ filter (`elem` allowed) str

getRowCol :: Int -> (Int, Int)
getRowCol len = (row, col)
    where row = (round $ sqrt $ fromIntegral len) :: Int
          col = if row * row - len < 0 then row + 1 else row            

chunk :: String -> Int -> [String]
chunk xs row
    | length xs <= row = [xs]
    | otherwise = take row xs : chunk (drop row xs) row
    

encode' :: [String] -> String
encode' [] = []
encode' groups = map head (filter (not . null) groups) ++ encode' (map tail $ filter (not . null) groups)

join :: [String] -> String
join [] = []
join [x] = x
join xs = head xs ++ " " ++ join (tail xs)

-- trim :: String -> String
-- trim = let t = reverse . dropWhile (== ' ') 
--        in t . t

pad :: String -> Int -> String
pad xs size  = reverse $ replicate size ' ' ++ reverse xs


encode :: String -> String
encode xs = encoded
    where cleaned = normalize xs
          (row, col) = getRowCol $ length cleaned
          grouped = pad' (chunk cleaned col)
          encoded = join $ pad' $ chunk (encode' grouped) row
          
          pad' = map (\x -> pad x (row - length x))
    