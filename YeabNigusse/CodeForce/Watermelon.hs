-- Problem  https://codeforces.com/contest/4/problem/A

main :: IO ()
main = 
    interact $
     watermelon .read . head .lines

watermelon :: Int -> String
watermelon n 
           | n == 2 = "NO"
           | odd n = "NO"
           | otherwise = "YES"