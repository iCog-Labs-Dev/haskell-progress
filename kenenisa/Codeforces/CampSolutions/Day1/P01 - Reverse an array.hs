-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196630081
main :: IO ()
main = do 
  n <- getLine
  a <- getLine
  putStrLn $ unwords (reverse (words a))
