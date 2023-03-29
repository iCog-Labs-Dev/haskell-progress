module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs 
     | xs == "G" = Right "C"
     | xs == "C" = Right "G"
     | xs == "T" = Right "A"
     | xs == "A" = Right "U"
     | xs == ""  = Right ""
     | length xs == 1 = Left (head xs)
     | otherwise = parseNuclotide' xs

parseNuclotide :: String -> String
parseNuclotide "" = "" 
parseNuclotide (x:xs)
                | x == 'G' = 'C': parseNuclotide xs
                | x == 'C' = 'G': parseNuclotide xs
                | x == 'T' = 'A': parseNuclotide xs
                | x == 'A' = 'U': parseNuclotide xs
   
                
isFound :: Char -> String -> Bool
isFound x text = x `elem` text

allFound :: String -> String-> Bool
allFound "" _ = True
allFound (x:xs) text = if isFound x text then allFound xs text else False

parseNuclotide' :: String -> Either Char String
parseNuclotide' xs = if allFound xs "GCTA" then  Right (parseNuclotide xs) else Left (findChar xs "GCTA")
                
                
findChar :: String -> String -> Char
findChar (x:xs) text 
          | isFound x text = findChar xs text
          | otherwise = x