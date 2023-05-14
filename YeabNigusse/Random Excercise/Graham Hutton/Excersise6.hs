fac :: Int -> Int
fac n 
      | n == 0 = 1
      | n < 0 = n
      |otherwise = product [1..n]
