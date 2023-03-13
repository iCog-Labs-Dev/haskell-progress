
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197225008

sieve :: [Int] -> [Int]
sieve [] = []
sieve (p:ps) = p : sieve [x | x <- ps, x `mod` p /= 0]

primes :: Int -> [Int]
primes n = sieve [2..n]

isAlmostPrime :: Int -> [Int]-> Bool
isAlmostPrime num pr = almostPrime num pr 0
    where 
        almostPrime _ [] 2 = True
        almostPrime _ [] _ = False
        almostPrime _ _ 3 = False
        almostPrime n (x:xs) p 
            | x >= n = p == 2
            | p > 2 = False
            | n `mod` x == 0 = almostPrime n xs (p+1)
            | otherwise = almostPrime n xs p

main :: IO ()
main = do
    n <- fmap read getLine
    let pr = primes n
    print $ foldr (\x acc -> if isAlmostPrime x pr then acc+1 else acc) 0 [1..n]