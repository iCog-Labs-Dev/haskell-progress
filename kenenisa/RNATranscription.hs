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
