
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197243708

import Data.List (intersperse)

main :: IO ()
main = do
  n <- fmap read getLine
  if even n
    then do
      let x = div n 2
      print x
      putStrLn $ intersperse ' ' $ replicate x '2'
    else do
      let x = div (n - 3) 2
      print $ x + 1
      putStrLn $ intersperse ' ' $ replicate x '2' ++ "3"