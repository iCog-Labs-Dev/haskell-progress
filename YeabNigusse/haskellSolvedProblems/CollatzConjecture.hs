module CollatzConjecture (collatz) where

collatz :: Integer -> Maybe Integer
collatz n
  | n <= 0    = Nothing
  | n == 1    = Just 0
  | otherwise = returnLength n
  

coll :: Integer -> [Integer]
coll n 
  | n <= 0 = error "Input Must be positive"
  | n == 1 = [1]
  | even n = n: coll (n `div` 2)
  | odd n  = n: coll (n*3 + 1)

returnLength :: Integer -> Maybe Integer
returnLength n 
          | n > 1 = Just (fromIntegral (length (coll n) - 1))
          | otherwise = Nothing
