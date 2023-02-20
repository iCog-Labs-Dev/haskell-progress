
module SumOfMultiples (sumOfMultiples) where
    
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum [num | num <- [1..limit],num < limit, isFactor num factors]
                                where isFactor n xs = case xs of [] -> False
                                                                 (x:xs) -> (x > 0 && (n `mod` x == 0)) || isFactor n xs
