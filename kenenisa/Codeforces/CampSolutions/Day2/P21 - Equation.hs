
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197486645


import Data.List

toint :: String -> Int
toint x = read x ::Int


main :: IO ()
main = do
  n <- fmap toint getLine
  if n == 1 then do
    putStrLn "9 8"
  else do
    putStrLn $  show (4*n) ++ " " ++ show (3*n)