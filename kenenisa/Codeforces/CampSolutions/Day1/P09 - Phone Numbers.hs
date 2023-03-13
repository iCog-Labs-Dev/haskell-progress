
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196661035

eightCount :: String -> Int
eightCount [] = 0
eightCount (x:xs)
  | x == '8' = 1 + eightCount xs
  | otherwise = eightCount xs

main :: IO ()
main = do
  n <- getLine
  line <- getLine
  print $ min (eightCount line) (length line `div` 11)
