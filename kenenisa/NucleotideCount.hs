module DNA (nucleotideCounts, Nucleotide(..)) where
import Data.Map (Map)
import qualified Data.Map as Map
data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotide :: [Char]
nucleotide = "ATGC"

isValidSeq :: [Char] -> Bool
isValidSeq [] = True
isValidSeq (x : xs) 
    | x `notElem` nucleotide = False
    | otherwise = isValidSeq xs

produceSeq :: Num b => [Char] -> [(Nucleotide, b)] -> [(Nucleotide, b)]
produceSeq [] seq = seq
produceSeq (x:xs) seq@[a,c,g,t] 
  | x == 'A' = produceSeq xs [(A,snd a + 1),c,g,t]
  | x == 'C' = produceSeq xs [a,(C,snd c + 1),g,t]
  | x == 'G' = produceSeq xs [a,c,(G,snd g + 1),t]
  | x == 'T' = produceSeq xs [a,c,g,(T,snd t + 1)]
  |otherwise = seq
  
nucleotideCounts :: String -> Either String (Map Nucleotide Int)
nucleotideCounts xs 
  | not (isValidSeq xs) = Left "Invalid"
  | otherwise = Right (Map.fromList (produceSeq xs [(A,0),(C,0),(G,0),(T,0)]))
