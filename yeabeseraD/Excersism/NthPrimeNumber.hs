module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (primeNumbers !! (n-1))

primeNumbers = [a | a <- [2..], isPrime a]

isPrime :: Integer ->Bool
isPrime a
    | a < 4 = True
    | even a = False
    | otherwise  = null $ factors a
    where factors a = filter (isFactor a) [b | b <- [2..(a-1)], odd b]
          isFactor a b = a `mod` b == 0