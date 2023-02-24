module CryptoSquare (encode) where

import Data.Char (toLower)

letters :: String
letters = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0'..'9']

normalize :: String -> String
normalize [] = []
normalize (x : xs)
  | x `elem` letters = toLower x : normalize xs
  | otherwise = normalize xs

floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

findRectangleColumn :: Int -> Int
findRectangleColumn len
  | sn * sn >= len =  sn
  | otherwise = sn + 1
  where
    sn = floorSqrt len

produceRectangle :: Int -> String -> [String]
produceRectangle _ [] = []
produceRectangle c xs = fitToRow : produceRectangle c (drop c xs)
                    where taken = take c xs
                          fitToRow = taken ++ replicate (c - length taken) ' '

cryptRow :: Int -> [[Char]] -> [Char] -> [Char]
cryptRow _ [] result = result
cryptRow cx (x:xs) result =  cryptRow cx xs (result ++ [x !! cx])

produceCrypt :: Int -> [[Char]] -> [Char]
produceCrypt c xs = concatMap (\cx -> cryptRow cx xs [] ++ " ") [0..(c-1)]

encode :: String -> String
encode [] = []
encode [x] = [toLower x]
encode xs = init out
        where
              normalized = normalize xs
              c = findRectangleColumn $ length normalized
              producedRect = produceRectangle c normalized
              out =  produceCrypt c producedRect
