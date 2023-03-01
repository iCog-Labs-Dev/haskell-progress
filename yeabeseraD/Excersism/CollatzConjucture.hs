module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n = let size = Just (fromIntegral(length (collatzSeq n)) - 1)
    in if size == Just (-1) then Nothing else size

collatzSeq :: Integer -> [Integer]
collatzSeq 1 = [1]
collatzSeq n 
    | n < 1 = []
    | even n = n : collatzSeq (n `div` 2)
    | otherwise = n : collatzSeq (3*n +1)