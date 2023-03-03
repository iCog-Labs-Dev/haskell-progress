module DNA (nucleotideCounts, Nucleotide(..)) where

import Data.Map (Map, empty, insert)
import Data.List (group, sort)

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show, Read)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs = if isValidDNA xs 
        then Right $ sequenceDNA xs
        else Left "error"

isValidDNA :: String -> Bool
isValidDNA = all (\x -> x `elem` "ACGT")

sequenceDNA :: String -> Map Nucleotide Int
sequenceDNA xs= foldr (\(k,v) acc -> insert (read k :: Nucleotide) v acc) empty dnaList
    where groupedDNA = group . sort $ xs
          dnaList = map (\n -> ([head n], length n)) groupedDNA