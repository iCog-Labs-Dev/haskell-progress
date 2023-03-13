module PerfectNumbers (classify, Classification(..)) where

data Classification = Deficient | Perfect | Abundant deriving (Eq, Show)


factors :: Integral a => a -> [a]
factors n = [x | x <- [1..n-1], n `mod` x == 0]


classify :: Int -> Maybe Classification
classify n 
    | n <= 0 = Nothing
    | aliquot == n = Just Perfect
    | aliquot > n  = Just Abundant
    | aliquot < n  = Just Deficient    
    where aliquot = (sum . factors) n

