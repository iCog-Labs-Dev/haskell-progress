-- https://exercism.org/tracks/haskell/exercises/difference-of-squares/

module Squares (difference, squareOfSum, sumOfSquares) where

difference :: Integral a => a -> a
difference n = (squareOfSum n) - (sumOfSquares n)

squareOfSum :: Integral a => a -> a
squareOfSum n = floor $ fromIntegral (n^2 * (n + 1)^2) / 4

sumOfSquares :: Integral a => a -> a
sumOfSquares n = floor $ fromIntegral (n * (n + 1) * (2 * n + 1)) / 6
