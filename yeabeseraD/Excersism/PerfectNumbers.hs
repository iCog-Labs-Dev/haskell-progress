module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify n
  | n < 1 = Nothing
  | alSum == n = Just Perfect
  | alSum > n = Just Abundant
  | alSum < n = Just Deficient
  where alSum = aliquotSum n

aliquotSum :: Int -> Int
aliquotSum n = let factors = [a | a <- [1..(n-1)], n `mod` a == 0]
  in sum factors
