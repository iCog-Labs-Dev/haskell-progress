module Grains (square, total) where

square :: Integer -> Maybe Integer
square n 
       | n <= 0 = Nothing
       | n > 64 = Nothing
       | otherwise = Just $ myList !! fromIntegral (n-1)

total :: Integer
total = foldl (\x acc -> x + acc) 0 myList


myList :: [Integer]
myList =  map (2^) [0 .. 63]
