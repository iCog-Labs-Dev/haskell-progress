-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture

module CollatzConjecture (collatz) where

collatzList :: Integer -> [Integer]
collatzList n
  | n == 1 = []
  | n `mod` 2 == 0 = n : collatzList ( floor $ (fromIntegral n) / 2)
  | otherwise = n : collatzList (3 * n + 1)

collatz :: Integer -> Maybe Integer
collatz n
    | n >= 1 = Just . fromIntegral . length $ collatzList n
    | otherwise = Nothing