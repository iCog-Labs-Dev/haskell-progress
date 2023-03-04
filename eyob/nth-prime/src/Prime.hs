module Prime (nth) where

isPrime :: Integer -> Bool
isPrime n = null [x | x <- [2 .. n - 1], n `mod` x == 0]

getPrimes :: [Integer]
getPrimes = filter isPrime [2 ..]

nth :: Int -> Maybe Integer
nth n
  | n < 1 = Nothing
  | otherwise = Just (snd (take n (zip [1 ..] getPrimes) !! (n - 1)))
