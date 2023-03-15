
-- TLE solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197487660

import Data.List (intercalate)

toint :: String -> Int
toint x = read x ::Int

sieve :: [Int] -> [Int]
sieve [] = []
sieve (x:xs) = x : sieve [y | y <- xs, mod y x /= 0]

primes :: Int -> [Int]
primes n = sieve [2..n]

main :: IO ()
main = do
  n <- fmap toint getLine
  let p = primes n
  print $ length p
  putStrLn $ unwords (map show p)