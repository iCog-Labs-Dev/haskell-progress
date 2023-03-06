module Histogram where

import qualified Data.Map as Map


fromJust :: Maybe Integer -> Integer
fromJust Nothing = 0
fromJust (Just a) = a


counter :: [Integer] -> Map.Map Integer Integer
counter [] = Map.fromList [(x, 0) | x <- [0..9]]
counter (x:xs) = Map.insert x (xCount + 1) count
    where count = counter xs
          xCount = fromJust $ Map.lookup x count


buildHistogram :: Map.Map Integer Integer -> [[Char]]
buildHistogram count
    | sum current > 0 = hLine : buildHistogram updatedCount
    | otherwise = []

    where current = [if freq > 0 then 1 else 0 | x <- [0..9], 
                            let freq = fromJust $ Map.lookup x count]  

          hLine = map (\x -> if x == 0 then ' ' else '*') current

          updatedCount = Map.fromList [(x, fromJust (Map.lookup x count) - 1) | x <- [0..9]] 


histogram :: [Integer] -> String
histogram xs = unlines $ reverse (buildHistogram $ counter xs) ++ ["==========\n0123456789\n"]

                                                           
main :: IO ()
main = do
    putStrLn $ histogram [1,1,1,1,1,2,3,4,5,6,6,7,8,9]