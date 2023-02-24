module CollatzConjecture (collatz) where

-- Recording each step and returning the number of steps
collatzSteps :: Integer -> [Int] -> Maybe Integer
collatzSteps n steps
    | n < 1 = Nothing
    | n == 1 = Just $ toInteger (length steps)
    | even n = collatzSteps (n `div` 2) ((fromIntegral n):steps)
    | odd n = collatzSteps (n*3 + 1) ((fromIntegral n):steps)

-- Alternative solution by counting esch step
collatzCount :: Integer -> Integer -> Maybe Integer
collatzCount n count
    | n < 1 = Nothing
    | n == 1 = Just count
    | even n = collatzCount (n `div` 2) (count + 1)
    | odd n = collatzCount (n*3 + 1) (count + 1)

collatz :: Integer -> Maybe Integer
collatz n = collatzSteps n []
-- collatz n = collatzCount n 0