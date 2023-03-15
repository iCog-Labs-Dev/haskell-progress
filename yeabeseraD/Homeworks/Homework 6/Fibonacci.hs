fib :: Integer -> Integer
fib n = fibs1 !! fromInteger n

fibs1 :: [Integer]
fibs1 = 0:1:zipWith (+) fibs1 (tail fibs1)