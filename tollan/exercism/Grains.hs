module Grains (square, total) where

calcGrainNumber :: Integer -> Integer -> Integer -> Integer
calcGrainNumber idx grains sqr =
        if (idx < sqr)
        then calcGrainNumber (idx+1) (grains*2) sqr
        else grains

square :: Integer -> Maybe Integer
square n
    | not (n `elem` [1..64]) = Nothing
    | otherwise = Just (calcGrainNumber 1 1 n)

total :: Integer
total = sum $ map (calcGrainNumber 1 1) [1..64]