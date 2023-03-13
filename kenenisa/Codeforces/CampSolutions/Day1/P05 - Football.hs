
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196643208

isDangerous :: String -> Char -> Int -> String
isDangerous [] _ count 
  | count == 7 = "YES"
  | otherwise = "NO"
isDangerous (x:xs) cur count 
  | count == 7 = "YES"
  | cur == x = isDangerous xs cur (count+1)
  | otherwise = isDangerous xs x 1
main :: IO ()
main = do
  s <- getLine
  putStrLn $ isDangerous (tail s) (head s) 1
