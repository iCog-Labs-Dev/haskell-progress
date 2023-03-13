-- https://exercism.org/tracks/haskell/exercises/pascals-triangle/

module Triangle (rows) where


addTwo :: [Integer] -> [Integer]
addTwo [] = []
addTwo [x] = []
addTwo (x:xs) = (x + head xs) : addTwo xs


row :: Int -> [Integer]
row 1 = [1]
row n = [1] ++ addTwo (row (n-1)) ++ [1]

rows :: Int -> [[Integer]]
rows = reverse . buildRows 
    where buildRows 0 = []
          buildRows x = row x : buildRows (x - 1)


