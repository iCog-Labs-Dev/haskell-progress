-- Problem https://codeforces.com/contest/1399/problem/A

-- main :: IO ()
-- main = 
--     interact $ unlines . removeEvens . tail .map ( check . map read . words ). lines


-- removeEvens :: [a] -> [a]
-- removeEvens xs = [y | (x,y) <- zip [0 ..] xs, odd x]

-- check :: (Ord a, Num a) => [a] -> String
-- check [] = "NO"
-- check xs 
--         |length (filter (\ (x,y) -> abs x-y > 1)   [(x,y)| x <- xs, y <- xs])  <= 1 = "YES"
--         | otherwise = "NO"

import Control.Arrow((>>>))
import Data.List
main = 
    interact $
    lines 
    >>> drop 1
    >>> process
    >>> unlines
process :: [String] -> [String]
process [] = []
process (_:xs:xss) = res : process xss
                     where  xs' = map read $ words xs
                            ans = solve xs'
                            res = if ans then "YES" else "NO"
                   

solve :: [Int] -> Bool
solve [] = False
solve xs = maximum gaps <= 1
       where xs' = sort xs
             gaps = zipWith (-) (tail xs') xs'
