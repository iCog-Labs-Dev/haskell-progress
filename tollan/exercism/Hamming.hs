module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
    | length xs /= length ys = Nothing
    | otherwise = Just hamming
    where tuples = zip xs ys
          hamming = length $ filter (\(x,y) -> x/=y) tuples
