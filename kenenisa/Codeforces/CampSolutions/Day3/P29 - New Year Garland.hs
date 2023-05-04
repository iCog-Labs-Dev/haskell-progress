-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203248386

import Data.List(sort)
toInt :: String -> Int
toInt x = read x :: Int
tc :: Int -> IO()
tc 0 = return ()
tc n = do 
  numbers <- fmap (map toInt . words) getLine
  let s = sort numbers
  if sum (init s) + 1 < last s then do
    putStrLn "No"
  else do
    putStrLn "Yes"
  tc (n-1)
main :: IO ()
main = do
  n <- fmap toInt getLine
  tc n