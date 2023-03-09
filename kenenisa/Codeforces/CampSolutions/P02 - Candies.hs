
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196633857

getIntList :: String -> [Int]
getIntList = map read . words
convertToStrList :: [Int] -> String
convertToStrList = unwords . map show
main :: IO ()
main = do
  n <- getLine
  a <- getLine
  b <- getLine
  let c = getIntList a
      (x:y:xs) = getIntList b
  print (sum  $ take (1+y-x) (drop x c))
