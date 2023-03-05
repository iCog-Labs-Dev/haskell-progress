module Prime (nth) where
import Language.Haskell.TH (prim)

nth :: Int -> Maybe Integer
nth n = Just (primeNumbers !! (n-1))

primeNumbers :: [Integer]
primeNumbers = [a | a <- [2..], and $ isPrime a ]

isPrime :: Integer -> [Bool]
isPrime a = foldl (\acc x -> (a `mod` x == 0) : acc ) [] [2..(a-1)]