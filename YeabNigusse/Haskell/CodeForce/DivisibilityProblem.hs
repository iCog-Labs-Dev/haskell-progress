
-- Problem https://codeforces.com/contest/1328/problem/A

main :: IO ()
main = 
    interact $ unlines .tail. map ((show . moves) . map read . words) . lines

moves :: [Int] -> Int
moves [a , b]
        | a `mod` b == 0 = 0
        | otherwise = b - (a `mod` b)
