module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA (x:xs)
    | x `notElem` "GCTA" = Left x
    | otherwise = Right (map change (x:xs))

change :: Char -> Char
change 'G' = 'C'
change 'C' = 'G'
change 'T' = 'A'
change 'A' = 'U'