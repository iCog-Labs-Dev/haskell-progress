module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
  | n <= 0 || n > 64 = Nothing
  | otherwise = Just (2 ^ (n - 1))

total :: Integer
total = sum (map (2 ^) [0 .. 63])

-- total = sum ([2 ^ n | n <- [0 .. 63]])