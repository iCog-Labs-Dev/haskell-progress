module Hamming (distance) where

distance :: String -> String -> Maybe Int
distance xs ys 
            | length xs /= length ys = Nothing
            | otherwise = Just (difference xs ys)



difference :: Eq a => [a] -> [a] -> Int
difference xs ys = length [x | (x, y) <- zip xs ys, x /= y]