-- https://exercism.org/tracks/haskell/exercises/nth-prime
module Prime (nth) where

nth :: Int -> Maybe Integer
nth n
    | n > 0 = Just (primes !! (n - 1))
    | otherwise = Nothing

primes :: [Integer]
primes = filterPrimes [2..]
    where filterPrimes (x:xs) = x : filterPrimes [n | n <- xs, n `mod` x > 0]