module SumOfMultiples (sumOfMultiples) where
import Data.List ( nub )
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum' factors limit

getList :: Integer -> [Integer] -> [Integer]
getList con [] = []
getList con (x:xs) = takeWhile (<con) (map (*x) [1 .. con-1]) ++ (getList con xs)

sum' :: [Integer] -> Integer -> Integer
sum' xs con = foldl (\x acc -> x + acc) 0 $ nub $ getList con xs