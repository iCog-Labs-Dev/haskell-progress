-- https://exercism.org/tracks/haskell/exercises/triangle/

module Triangle (TriangleType(..), triangleType) where

import Data.List (sort)

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

sortedSides :: (Num a, Ord a) => [a] -> (a, a, a)
sortedSides xs = (s !! 0, s !! 1, s !! 2)
    where s = sort xs



triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c
    | z >= x + y = Illegal
    | x == y && y == z = Equilateral
    | x /= y && x /= z && y /= z = Scalene
    | otherwise = Isosceles

    where (x, y, z) = sortedSides [a, b, c]
