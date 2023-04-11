roundx :: Int -> Int
roundx x 
       | x >= 38 && (mx - x) < 3 = mx
       | otherwise = x
       where mx = x + (5 - x `mod` 5)
solve :: [Int] -> [Int]
solve  = map roundx 

main :: IO ()
main = interact $ unlines . map show .solve . map read . tail . words 