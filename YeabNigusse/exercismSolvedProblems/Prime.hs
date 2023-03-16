module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
     | n <= 0 = Nothing
     | otherwise = Just (fromIntegral (primeNumbers n !! (n-1)))

isPrime :: Int -> Bool
isPrime n = n > 1 && all (\x -> n `mod` x /= 0) [2..floor (sqrt (fromIntegral n))]

primeNumbers :: Int -> [Int]
primeNumbers n = take n (2 : filter isPrime [3,5..])