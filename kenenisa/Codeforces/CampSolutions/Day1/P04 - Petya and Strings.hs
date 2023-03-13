
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196639954

import Data.Char (ord,toLower)

comp :: [Char] -> [Char] -> String
comp [] [] = "0"
comp (x:xs) (y:ys) | ord x > ord y = "1"
                   | ord x < ord y = "-1"
                   | otherwise = comp xs ys

main :: IO ()
main = do 
  f <- getLine
  s <- getLine
  putStrLn $ comp (map toLower f) (map toLower s) 
