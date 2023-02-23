module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c | a + b < c || a + c < b || b + c < a || any (==0) [a,b,c] = Illegal
                   | a == b && b == c = Equilateral
                   | a /= b && b /= c && a /= c = Scalene
                   | otherwise = Isosceles
