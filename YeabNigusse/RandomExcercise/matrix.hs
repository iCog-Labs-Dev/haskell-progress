
main = do
    putStrLn "Enter row number"
    row <- getLine
    putStrLn "Enter column number"
    column <- getLine
    putStrLn "row number is and column number is " 

getmatrix :: (Int, Int)  -> String
getmatrix (a, b) = unlines $ map row [1 .. a]
                       where row n = unwords $ map column [1 .. b]
                              where column m = show (n * m + m - n)

getlist :: Int -> [[Int]]
getlist a = map row [0 .. a-1]
                where row n = map column [0 .. n]
                       where column m = factorial n `div` (factorial m * factorial (n-m))

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n*factorial (n-1)
