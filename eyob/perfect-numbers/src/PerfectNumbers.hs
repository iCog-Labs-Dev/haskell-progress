module PerfectNumbers (classify, Classification (..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n <= 0 = Nothing
  | sum (factors n) == n = Just Perfect
  | sum (factors n) < n = Just Deficient
  | sum (factors n) > n = Just Abundant

factors :: Int -> [Int]
factors 0 = []
factors n = [x | x <- [1 .. n - 1], n `mod` x == 0]