module LocalMaxima where

localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x : y : z : xs)
  | x < y && y > z = y : localMaxima (z : xs)
  | otherwise = localMaxima (y : z : xs)
localMaxima (x : xs) = []
