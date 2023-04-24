toInt :: String -> Int
toInt x = read x :: Int

floorSqrt :: Int -> Int
floorSqrt = ceiling . sqrt . fromIntegral 


factors :: Int -> Bool
factors n = length [x | x <- [1..(floorSqrt n)], mod n x == 0] == 2

printLines :: [Int] -> IO ()
printLines [] = return ()
printLines (x:xs) = do 
    putStrLn $ if factors x then "YES" else "NO"
    printLines xs

main :: IO ()
main = do 
    n <- fmap toInt getLine
    nums <- fmap (map toInt . words) getLine
    printLines nums

