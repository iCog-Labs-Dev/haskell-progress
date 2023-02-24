module SumOfMultiples (sumOfMultiples) where
import Data.List

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ nub $ concat $ map (multiples limit) filteredFactors
                            where filteredFactors = filter (>0) factors 


multiples :: Integer -> Integer -> [Integer]
multiples limit x = takeWhile (< limit) $ map (* x) [1..]