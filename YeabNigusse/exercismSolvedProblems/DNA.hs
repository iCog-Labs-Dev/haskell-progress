module DNA (nucleotideCounts, Nucleotide(..)) where

--import Data.Map (Map)
import qualified Data.Map as Map
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts xs 
                 | not (allFound xs alphabet) = Left "error"
                 | otherwise = Right $ insertNucleotides xs



nucloNumber :: Map.Map Nucleotide Int
nucloNumber = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]

insertKey :: Nucleotide -> Int -> Map.Map Nucleotide Int -> Map.Map Nucleotide Int
insertKey key num nucloNumber = Map.insertWith (+) key num nucloNumber 

insertNuclotide :: Char -> Map.Map Nucleotide Int -> Map.Map Nucleotide Int
insertNuclotide 'A' m = insertKey A 1 m
insertNuclotide 'C' m = insertKey C 1 m
insertNuclotide 'G' m = insertKey G 1 m
insertNuclotide 'T' m = insertKey T 1 m


insertNucleotides :: String -> Map.Map Nucleotide Int
insertNucleotides = foldr insertNuclotide nucloNumber
  where nucloNumber = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
        
isFound :: Char -> String -> Bool
isFound x text = x `elem` text
   
alphabet :: [Char]
alphabet = "AGTC"
  
allFound :: String -> String-> Bool
allFound "" _ = True
allFound (x:xs) text = if isFound x text then allFound xs text else False
