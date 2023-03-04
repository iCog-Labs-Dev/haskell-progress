module DNA (nucleotideCounts, Nucleotide (..)) where

import Data.Char (intToDigit)
import Data.List (group, sort)
import Data.Map (Map, fromList)

-- import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs =
  if any (`notElem` "ACGT") xs
    then Left $ error "Invalid nucleotide in strand"
    else Right $ fromList $ map (\(x, y) -> (convertNucleotide x, y)) $ letterCount xs

convertNucleotide :: Char -> Nucleotide
convertNucleotide x = case x of
  'A' -> A
  'C' -> C
  'G' -> G
  'T' -> T

letterCount :: [Char] -> [(Char, Int)]
letterCount xs = map (\x -> (head x, length x)) (group (sort xs))