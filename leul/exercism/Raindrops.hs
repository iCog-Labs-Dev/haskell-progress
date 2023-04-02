-- https://exercism.org/tracks/haskell/exercises/raindrops/edit

module Raindrops (convert) where


divisibleBy3 :: Integral a => a -> String
divisibleBy3 n 
    | n `mod` 3 == 0 = "Pling"
    | otherwise = ""

divisibleBy5 :: Integral a => a -> String
divisibleBy5 n 
    | n `mod` 5 == 0 = "Plang"
    | otherwise = ""

divisibleBy7 :: Integral a => a -> String
divisibleBy7 n 
    | n `mod` 7 == 0 = "Plong"
    | otherwise = ""


convert :: Int -> String
convert n = if null output then show n else output
    where output = divisibleBy3 n ++ divisibleBy5 n ++ divisibleBy7 n    
