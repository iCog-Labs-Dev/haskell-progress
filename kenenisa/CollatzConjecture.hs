module CollatzConjecture (collatz) where
collat :: (Integral a, Num t) => a -> t -> t
collat 1 x = x
collat n x | even n    = collat (n `div` 2) (x+1) 
           | odd n     = collat ((3*n)+1) (x+1) 
           | otherwise = x
collatz :: Integer -> Maybe Integer
collatz n | n==1      = Just 0 
          | n <1      = Nothing 
          | otherwise = Just (collat n 0)
