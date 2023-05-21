fac :: Int -> Int
fac n 
      | n == 0 = 1
      | n < 0 = n
      |otherwise = product [1..n]

sumdown :: Int -> Int
sumdown 0 = 0
sumdown n = n + sumdown (n-1)

(^) :: (Eq p, Num p) => p -> p -> p
m^1 = m
m^n = m * (m*(n-1))