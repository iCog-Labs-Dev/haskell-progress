-- import Data.List (intersperse)

-- convertToList :: String -> String -> String -> String -> String
-- convertToList [] ones twos threes =  ones ++ twos ++ threes
-- convertToList (x:xs) ones twos threes 
--     | x == '1' = convertToList xs (x:'+':ones) twos threes
--     | x == '2' = convertToList xs ones (x:'+':twos) threes
--     | x == '3' = convertToList xs ones twos (x:'+':threes)
--     | otherwise = convertToList xs ones twos threes

-- main :: IO ()
-- main = do 
--     line <- getLine
--     putStrLn $ init $ convertToList line [] [] []

-- alternative approach

import Data.List (sort,intersperse)

convertToList :: String -> [Int]
convertToList [] = []
convertToList ('+':xs) = convertToList xs
convertToList (x:xs) = read [x] : convertToList xs

main :: IO ()
main = do
    line <- getLine
    putStrLn $ intersperse '+' $ map (head . show) $ sort $ convertToList line

