module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys
    | length xs /= length ys = Nothing
    | otherwise = Just $ calculateDistance xs ys

calculateDistance :: String -> String -> Int
calculateDistance xs ys = foldl (\acc x -> if x then acc + 1 else acc ) 0 (zipWith (/=) xs ys)