module Prime (nth) where

nth :: Int -> Maybe Integer
nth 0 = Nothing
nth n = Just (primeNumbers !! (n-1))

primeNumbers = [a | a <- [2..], not $ isPrime a]

isPrime x = not $ all (\p -> x `mod` p /= 0) possibleFactors
    where possibleFactors = takeWhile (\p -> p*p <= x) [2..(x-1)]
        
