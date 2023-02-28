module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)

classify :: Int -> Maybe Classification
classify num
    | num < 1 = Nothing
    | aliquot == num = Just Perfect
    | aliquot > num = Just Abundant
    | otherwise = Just Deficient
    where aliquot = aliquotSum num

aliquotSum :: Int -> Int
aliquotSum num = sum $ filter (\x -> num `mod` x == 0) [1..(num-1)] 