module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA [] = Right []
toRNA ('G':xs) =  ('C':) <$> toRNA xs
toRNA ('C':xs) =  ('G':) <$> toRNA xs
toRNA ('T':xs) =  ('A':) <$> toRNA xs
toRNA ('A':xs) =  ('U':) <$> toRNA xs
toRNA (x:xs) = Left x