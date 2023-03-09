
-- accepted at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196637761

getIntList :: String -> [Int]
getIntList = map read . words
convertToStrList :: [Int] -> String
convertToStrList = unwords . map show

abbr :: [Char] -> [Char]
abbr w | length w  > 10 = head w : show (length w - 2) ++ [last w]
       | otherwise = w
testCases :: Int -> IO ()
testCases 0 = return ()
testCases t = do
      w <- getLine
      putStrLn $ abbr w
      testCases (t-1)
main :: IO ()
main = do
  t <- getLine
  testCases (read t :: Int)
