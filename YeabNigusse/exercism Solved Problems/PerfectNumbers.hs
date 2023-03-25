module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
         | n < 1 = Nothing
         | isPerfect n = Just Perfect
         | isDeficient n = Just Deficient
         | isAbundant n = Just Abundant
         | otherwise =  Nothing

isPerfect :: Int -> Bool
isPerfect n = (sum [x | x <- [1 .. n-1], n `mod` x == 0]) == n

isDeficient :: Int -> Bool
isDeficient n = (sum [x | x <- [1 .. n-1], n `mod` x == 0]) < n

isAbundant :: Int -> Bool
isAbundant n = (sum [x | x <- [1 .. n-1], n `mod` x == 0]) > n