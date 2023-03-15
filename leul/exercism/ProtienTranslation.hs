-- https://exercism.org/tracks/haskell/exercises/protein-translation/

module ProteinTranslation(proteins) where

-- AUG	Methionine
-- UUU, UUC	Phenylalanine
-- UUA, UUG	Leucine
-- UCU, UCC, UCA, UCG	Serine
-- UAU, UAC	Tyrosine
-- UGU, UGC	Cysteine
-- UGG	Tryptophan
-- UAA, UAG, UGA	STOP


codonToProtein :: String -> Maybe String
codonToProtein codon    
    | codon `elem` ["AUG"] = Just "Methionine"
    | codon `elem` ["UUU", "UUC"] = Just "Phenylalanine"                             
    | codon `elem` ["UUA", "UUG"] = Just "Leucine"
    | codon `elem` ["UCU", "UCC", "UCA", "UCG"] = Just "Serine"
    | codon `elem` ["UAU", "UAC"] = Just "Tyrosine"
    | codon `elem` ["UGU", "UGC"] = Just "Cysteine"
    | codon `elem` ["UGG"] = Just "Tryptophan"
    | codon `elem` ["UAA", "UAG", "UGA"] = Just "STOP"
    | otherwise = Nothing


proteins' :: String -> [Maybe String]
proteins' [] = []
proteins' s = (codonToProtein (take 3 s)) : proteins' (drop 3 s)


proteins :: String -> Maybe [String]
proteins s 
    | Nothing `elem` translated = Nothing
    | otherwise =  Just (takeWhile (/="STOP") $ map (\(Just w) -> w) translated)
    where translated  = proteins' s