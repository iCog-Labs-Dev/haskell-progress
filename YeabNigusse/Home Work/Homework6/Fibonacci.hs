-- Excersise1

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib n = fib (n-1) + fib (n-1)

fibs1 :: [Integer]
fibs1 = map fib [0 ..]

fibs :: [Integer]
fibs = 1 : 2 : zipWith (+) fibs (tail fibs)

-- TOPICS I NEED TO COVER
-- DYNAMIC PROGRAMMING IN IMPARATIVE AND FUNCTIONAL PROGRAMMING AND THEIR DIFFERENCE
-- WHAT IS EVEN DYNAMIC PROGRAMMING(EXPRESS ON YOUR OWN TERMS AND GIVE ME SOME EXAMPLE)
-- WHAT IS THE RELATIOSHIP BETWEEN DP AND LAZY EVALUASTION IN HASKELL

