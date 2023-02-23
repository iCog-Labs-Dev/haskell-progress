module Triangle (rows) where

rows :: Int -> [[Integer]]
rows x
  | x <= 0 = []
  | otherwise = rows (x - 1) ++ [row x]
  where
    row :: Int -> [Integer]
    row 1 = [1]
    row y = [1] ++ zipWith (+) (row (y - 1)) (tail (row (y - 1))) ++ [1]
