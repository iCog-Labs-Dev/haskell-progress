-- https://exercism.org/tracks/haskell/exercises/sum-of-multiples

module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [x | x <- [0..(limit-1)], 
                                let validFactors = filter (/=0) factors 
                                in any (==0) (map (x `mod`) validFactors) ]
