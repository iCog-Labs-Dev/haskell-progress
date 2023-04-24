-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203239811

import Data.List (sort)
toInt :: String -> Int
toInt x = read x :: Int


countHowMany :: [Int] -> Int -> Int
countHowMany [] _ = 0
countHowMany (x:xs) num
  | num >= x = 0
  | otherwise = 1 + countHowMany xs num

loop :: Int -> [Int] -> IO()
loop n numbers = do
    if n == 0 
      then do
        putStrLn $ unwords $ map show numbers
      else do
        num <- fmap toInt getLine
        print $ 1 + countHowMany numbers num
        loop (n-1) (reverse $ sort (num:numbers))


main :: IO ()
main = do 
  n <- fmap toInt getLine
  loop n []
 