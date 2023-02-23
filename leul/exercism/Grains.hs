-- https://exercism.org/tracks/haskell/exercises/grains

module Grains (square, total) where

square :: Integer -> Maybe Integer
square n
    | 1 <= n && n <= 64 = Just (2 ^ (n - 1))
    | otherwise = Nothing

total :: Integer
total = sum [2^(x) | x <- [0..63]]
