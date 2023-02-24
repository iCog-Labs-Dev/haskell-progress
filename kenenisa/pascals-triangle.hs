module Triangle (rows) where

row :: [Int] -> [Int]
row [] = []
row [x] = [x]
row (x:y:xs) = x+y : row (y:xs)

rows :: Int -> [[Int]]
rows 0 = []
rows 1 = [[1]]
rows 2 = [[1],[1,1]]
rows x = prevRow ++ [1 : row (last prevRow)]
        where prevRow = rows (x - 1)
