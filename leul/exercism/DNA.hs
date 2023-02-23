-- https://exercism.org/tracks/haskell/exercises/rna-transcription

module DNA (toRNA) where

fromJust :: Maybe a -> a
fromJust (Just a) = a

nucleotides :: String
nucleotides = "AGTC"

rnaComplement :: Char -> Char
rnaComplement 'A' = 'U'
rnaComplement 'T' = 'A'
rnaComplement 'G' = 'C'
rnaComplement 'C' = 'G'

findInvalid :: String -> Maybe Char
findInvalid [] = Nothing
findInvalid (x:xs)
    | not (x `elem` nucleotides) = Just x
    | otherwise = findInvalid xs

transcribe :: String -> String
transcribe [] = []
transcribe (x:xs) = rnaComplement x : transcribe xs


toRNA :: String -> Either Char String
toRNA xs
    | invalid /= Nothing = Left (fromJust invalid)
    | otherwise = Right $ transcribe xs
    where invalid = findInvalid xs


