module SumOfMultiples (sumOfMultiples) where

sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = 
  let factors' = [a | a <- factors, a /= 0]; listOfMultiples = filter (\x -> or (map (\y -> x `mod` y == 0) factors')) [1..(limit-1)]
  in sum listOfMultiples