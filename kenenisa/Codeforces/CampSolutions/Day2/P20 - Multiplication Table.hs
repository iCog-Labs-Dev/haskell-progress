
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197485384

import Data.List

toint :: String -> Int
toint x = read x ::Int

findMultiple :: Int -> Int -> Int -> Int
findMultiple _ 0 _ = 0
findMultiple n i x 
  | mod x i == 0 && div x i <= n = 1 + findMultiple n (i-1) x
  | otherwise = findMultiple n (i-1) x

main :: IO ()
main = do
  [n,x] <- fmap (map toint . words) getLine
  if n*n < x then do
    print 0
  else do
    print $ findMultiple n n x 