module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA xs
    | any (`notElem` "GCTA") xs = Left (head (filter (`notElem` "GCTA") xs))
    | otherwise = Right (map change xs)

change :: Char -> Char
change 'G' = 'C'
change 'C' = 'G'
change 'T' = 'A'
change 'A' = 'U'