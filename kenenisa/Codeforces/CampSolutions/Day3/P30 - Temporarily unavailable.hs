
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/204451219

import Data.List(sort)
toInt :: String -> Int
toInt x = read x :: Int
tc :: Int -> IO()
tc 0 = return ()
tc n = do
  numbers <- fmap (map toInt . words) getLine
  let [_,_,c,r] = numbers
      [a,b] = sort $ take 2 numbers
      start = c - r
      end = c + r
      left = max (start - a) 0
      right = max (b - end) 0 in print $ min (right + left) (b - a)
  tc (n-1)
main :: IO ()
main = do
  n <- fmap toInt getLine
  tc n