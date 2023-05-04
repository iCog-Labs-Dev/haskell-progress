
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203246223

import Data.List(sort)
toInt :: String -> Int
toInt x = read x :: Int
 
main :: IO ()
main = do 
  n <- fmap toInt getLine
  numbers <- fmap (map toInt . words) getLine
  putStrLn $ unwords $ map show (sort numbers)