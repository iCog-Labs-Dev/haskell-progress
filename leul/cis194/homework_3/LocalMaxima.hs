module LocalMaxima where
import Distribution.Compat.Lens (_1)


localMaxima :: [Integer] -> [Integer]
localMaxima [] = []
localMaxima (x:y:z:xs)
    | x < y && y > z = y : localMaxima (z:xs)
    | otherwise = localMaxima (y:z:xs)
localMaxima (x:xs) = []
