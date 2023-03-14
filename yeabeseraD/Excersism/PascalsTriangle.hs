module Triangle (rows) where
import Data.List

rows :: Int -> [[Integer]]
rows 0 = []
rows 1 = [[1]]
rows x = rows (x-1) ++ [nextRow]
    where currentRow = last $ rows (x-1)
          nextRow =  1 : calcNextRow currentRow ++ [1]

calcNextRow :: [Integer]->[Integer]
calcNextRow [] = []
calcNextRow [x] = []
calcNextRow [x,y] = [x+y]
calcNextRow (x:y:ys) = (x+y):calcNextRow (y:ys)