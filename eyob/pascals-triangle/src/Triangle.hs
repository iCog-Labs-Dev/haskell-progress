module Triangle (rows) where

import Data.List (elemIndex)

-- rows :: Int -> [[Integer]]
-- rows x
--   | x <= 0 = []
--   | otherwise = rows (x - 1) ++ [row x]
--   where
--     row :: Int -> [Integer]
--     row 1 = [1]
--     row y = [1] ++ zipWith (+) (row (y - 1)) (tail (row (y - 1))) ++ [1]

rows :: Int -> [[Int]]
rows x
  | x <= 0 = []
  | x == 1 = [[1]]
  | x == 2 = [[1], [1, 1]]
  | otherwise = rows (x - 1) ++ [normalizeRow ((rows (x - 1)) !! (x - 2))]
  where
    normalizeRow xs = [1] ++ row xs ++ [1]
    row :: [Int] -> [Int]
    row [] = []
    row (a : b : xs)
      | null xs = [a + b]
      | otherwise = a + b : row (b : xs)

-- row wholeX wholeY = [head wholeY] ++ zipWith (+) (row wholeX (tail wholeY)) (tail wholeY) ++ [last wholeY]

findElmtByIdx :: [Int] -> Int -> Int
findElmtByIdx [] _ = 0
findElmtByIdx xs idx
  | idx < 0 = 0
  | idx >= length xs = 0
  | otherwise = xs !! idx

replaceElmtAtIdx :: [Int] -> Int -> Int -> [Int]
replaceElmtAtIdx xs idx y = take idx xs ++ [y] ++ drop (idx + 1) xs

findIdx :: [Int] -> Int -> Int
findIdx xs elmt = fst $ head $ filter (\(_, x) -> x == elmt) (zip [0 ..] xs)