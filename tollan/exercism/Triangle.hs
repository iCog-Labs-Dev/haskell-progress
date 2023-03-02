module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
    | x < 1 = []
    | otherwise = rows (x-1) ++ [row x] 

row x
    | x == 1 = [1]
    | otherwise = pascalRow $ [0] ++ row (x-1) ++ [0]
    where pascalRow r = case r of
                    [0] -> []
                    (x:y:xs) -> (x+y) : (pascalRow (y:xs))