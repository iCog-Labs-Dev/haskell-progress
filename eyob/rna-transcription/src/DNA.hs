module DNA (toRNA) where

toRNA :: String -> Either Char String
toRNA xs
  | any (`notElem` "GCTA") xs = Left (head (filter (`notElem` "GCTA") xs))
  | otherwise = Right (map transcribe xs)
  where
    transcribe 'G' = 'C'
    transcribe 'C' = 'G'
    transcribe 'T' = 'A'
    transcribe 'A' = 'U'
