module Triangle (TriangleType(..), triangleType) where

data TriangleType = Equilateral
                  | Isosceles
                  | Scalene
                  | Illegal
                  deriving (Eq, Show)

triangleType :: (Num a, Ord a) => a -> a -> a -> TriangleType
triangleType a b c 
                | isTriangle a b c && isEquilateral a b c = Equilateral
                | isTriangle a b c && isIsosceles a b c  = Isosceles
                | isTriangle a b c && isScalene a b c = Scalene
                | otherwise = Illegal

isTriangle :: (Num a, Ord a) => a -> a -> a -> Bool
isTriangle a b c  = (a + b + c) > 0 && length
                    where length 
                            | a+b < c = False
                            | a+c < b = False
                            | c+b < a = False
                            | otherwise = True
isEquilateral :: (Num a, Ord a) => a -> a -> a -> Bool
isEquilateral a b c
                  | a ==b && a == c = True
                  | otherwise = False
isIsosceles :: (Num a, Ord a) => a -> a -> a -> Bool
isIsosceles a b c 
              | a == b || a ==c || c == b = True
              | otherwise = False
isScalene :: (Num a, Ord a) => a -> a -> a -> Bool
isScalene a b c 
             | a /=b && a /= c && c /= b = True
             | otherwise = False
                 
