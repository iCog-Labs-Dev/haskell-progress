-- Problem https://codeforces.com/contest/71/problem/A
main :: IO ()
main = 
    interact $
    unlines . tail . abbreviate . lines


abbreviate :: [String] -> [String]
abbreviate [] = []
abbreviate (x:xs)
      | length x > 10 = (take 1 x ++ show (length (tail (init x))) ++ [last x]) : abbreviate xs
      | otherwise = x : abbreviate xs
      