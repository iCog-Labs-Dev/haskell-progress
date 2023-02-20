module DNA (toRNA) where

nucleotide :: [Char]
nucleotide = "ACGTU"

convertToDNA :: [Char] -> [Char]
convertToDNA [] = []
convertToDNA (x:xs) 
    | x == 'G' = 'C' : convertToDNA xs 
    | x == 'C' = 'G' : convertToDNA xs 
    | x == 'T' = 'A' : convertToDNA xs 
    | x == 'A' = 'U' : convertToDNA xs 

isRNA :: [Char] -> Bool
isRNA xs = 'U' `elem` xs

isValidSeq :: [Char] -> [Char]
isValidSeq [] = ""
isValidSeq (x : xs) 
    | x `notElem` nucleotide = [x] 
    | otherwise              = isValidSeq xs

toRNA :: String -> Either Char String
toRNA xs 
    | null xs                    = Right "" 
    | not (null (isValidSeq xs)) = Left (head (isValidSeq xs))
    | isRNA xs                   = Left 'U' 
    | otherwise                  = Right (convertToDNA xs)