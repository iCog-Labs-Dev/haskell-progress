module Golf where
import qualified Data.List as List
skips :: [a] -> [[a]]
skips [] = []
skips val 
        | length val == 1 = [val]
        | otherwise = val : map row [2 ..  length val]
            where row n = [y | (x,y) <- zip [1 ..] val, x `mod` n == 0]

localMaxima :: [Integer] -> [Integer]
localMaxima xs = [b| (a,b) <- zip [0 ..] xs, (c,d) <- zip [0 ..] xs, a+1 == c && b > d] `List.intersect` [d| (a,b) <- zip [0 ..] xs, (c,d) <- zip [0 ..] xs, a+1 == c && b < d]

histogram :: [Integer] -> String
histogram xs = unlines $ map row [0 .. max + 1]
               where row n = unwords $ map column [0 .. 9]
                         where column m 
                                | n == max = "="
                                | n == max + 1 = show m
                                | (max - n) <= findKey m (pair xs) = "*"
                                | otherwise = ""
                     max = maximum [findLength x xs| x <- [0 .. 9]]

pair :: (Enum a, Enum t, Num a, Num t, Num b, Eq t) => [t] -> [(a, b)]
pair xs = zip [0 .. 9] [findLength x xs| x <- [0 .. 9]]

findLength :: (Num a, Eq t) => t -> [t] -> a
findLength _ [] = 0
findLength x (y:ys) 
         | y == x = 1+ findLength x ys
         | otherwise = findLength x ys

findKey :: (Eq k) => k -> [(k,v)] -> v
findKey key = snd .head . filter (\(k,v) -> k ==key)