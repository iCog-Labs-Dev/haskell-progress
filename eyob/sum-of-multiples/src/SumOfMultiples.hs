module SumOfMultiples (sumOfMultiples) where

import Data.List (nub)

multiples :: Integer -> [Integer]
multiples n = map (* n) [1 ..]

multiplesUpTo :: Integer -> Integer -> [Integer]
multiplesUpTo n limit = takeWhile (< limit) (multiples n)

getMultiplesOf :: [Integer] -> Integer -> [Integer]
getMultiplesOf xs limit = nub (concatMap (`multiplesUpTo` limit) xs)

-- Note: the above line is equivalent to the following but more concise:
-- getMultiplesOf xs limit = concat (map (`multiplesUpTo` limit) xs)
-- Note: the above line is equivalent to the following but more concise:
-- getMultiplesOf (x:xs) limit = concat (multiplesUpTo x limit:getMultiplesOf xs limit)
-- Note: the above line is equivalent to the following but more performant:
-- getMultiplesOf (x:xs) limit = concat (multiplesUpTo x limit ++ getMultiplesOf xs limit)

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples [] _ = 0
sumOfMultiples factors limit = sum (getMultiplesOf factors limit)
