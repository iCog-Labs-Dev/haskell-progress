module DNA (nucleotideCounts, Nucleotide(..)) where

import qualified Data.Map as Map

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

nucleotide = "ACGT"

isValidSequence :: String -> Bool
isValidSequence [] = True
isValidSequence (x:xs)
    | x `elem` nucleotide = isValidSequence xs
    | otherwise = False


fromJust :: Maybe a -> a
fromJust (Just a) = a


counter :: String -> (Map.Map Nucleotide Int)
counter [] = Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)]
counter (x:xs)
    | x == 'A' = Map.fromList [(A, 1 + cummulativeA), (C, cummulativeC), (G, cummulativeG), (T, cummulativeT)]
    | x == 'C' = Map.fromList [(A, cummulativeA), (C, 1 + cummulativeC), (G, cummulativeG), (T, cummulativeT)]
    | x == 'G' = Map.fromList [(A, cummulativeA), (C, cummulativeC), (G, 1 + cummulativeG), (T, cummulativeT)]
    | x == 'T' = Map.fromList [(A, cummulativeA), (C, cummulativeC), (G, cummulativeG), (T, 1 + cummulativeT)]
    where cummulativeA = fromJust $ Map.lookup A (counter xs) 
          cummulativeC = fromJust $ Map.lookup C (counter xs) 
          cummulativeG = fromJust $ Map.lookup G (counter xs) 
          cummulativeT = fromJust $ Map.lookup T (counter xs) 


nucleotideCounts :: String -> Either String (Map.Map Nucleotide Int)
nucleotideCounts xs
    | not (isValidSequence xs) = Left "Invalid"
    | otherwise = Right (counter xs)


