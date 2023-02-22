module CollatzConjecture (collatz) where

collatzCount :: (Integer, Integer) -> Integer
collatzCount (n, counter)
  | n == 1 = counter
  | even n = collatzCount (n `div` 2, counter + 1)
  | otherwise = collatzCount (3 * n + 1, counter + 1)

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0 = Nothing
  | otherwise = Just (collatzCount (n, 0))
