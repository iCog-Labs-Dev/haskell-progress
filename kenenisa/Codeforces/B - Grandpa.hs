count :: String -> String -> String
count reserve []
  | length reserve == 5 = "YES"
  | otherwise = "NO"
count reserve (x : xs)
  | length reserve == 5 = "YES"
  | x `notElem` reserve = count (x : reserve) xs
  | otherwise = count reserve xs
 
main :: IO ()
main = do
  a <- getLine
  putStrLn (count [] (map head (words a)))
