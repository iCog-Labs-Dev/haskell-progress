module Prime (nth) where

generatePrime :: [Integer]
generatePrime = primes [2..]
  where
    primes (x:xs) = x : primes [y | y <- xs, y `mod` x > 0]

nth :: Int -> Maybe Integer
nth n | n <= 0 = Nothing
      | otherwise = Just (generatePrime !! (n-1))
