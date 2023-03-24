module Triangle (rows) where

rows :: Int -> [[Int]]
rows a = map row [0 .. a-1]
                where row n = map column [0 .. n]
                       where column m = factorial n `div` (factorial m * factorial (n-m))

factorial :: (Eq t, Num t) => t -> t
factorial 0 = 1
factorial n = n*factorial (n-1)

