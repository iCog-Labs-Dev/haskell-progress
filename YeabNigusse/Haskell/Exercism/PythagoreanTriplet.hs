module Triplet (tripletsWithSum) where

tripletsWithSum :: Int -> [(Int, Int, Int)]
tripletsWithSum sum = [(a, b, c) | c <- [(sum `div` 3 + 1)..(sum `div` 2)],
                                   b <- [(sum - c - 1) `div` 2 + 1..min c (sum - c - 1)],
                                   let a = sum - b - c,
                                   a^2 + b^2 == c^2]
