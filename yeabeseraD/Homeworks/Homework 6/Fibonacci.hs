fib :: Integer -> Integer
fib 0 = 0
fib 1 = 1
fib x = fib (x-1) + fib (x-2)

fibs1 :: [Integer]
fibs1 = 