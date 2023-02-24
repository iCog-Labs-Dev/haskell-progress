module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map as Map (Map, fromList)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs
    | not $ all (`elem` ['A','C','T','G']) xs = Left "error"
    | otherwise = Right $ Map.fromList [(A, numA),(C, numC),(G, numG),(T, numT)]
    where numA = length [x | x <- xs, x == 'A']
          numC = length [x | x <- xs, x == 'C']
          numT = length [x | x <- xs, x == 'T']
          numG = length [x | x <- xs, x == 'G']
    