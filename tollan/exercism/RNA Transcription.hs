module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs = if (all (`elem` ['C','G','A','T']) xs)
           then Right trans
           else Left $ head [x | x <- xs, not $ x `elem` ['C','G','A','T']]
           where trans = (map convertNucleotide xs)

convertNucleotide :: Char -> Char
convertNucleotide x
    | x == 'G' = 'C'
    | x == 'C' = 'G'
    | x == 'T' = 'A'
    | x == 'A' = 'U'
    | otherwise = x
