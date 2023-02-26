-- https://exercism.org/tracks/haskell/exercises/acronym/

module Acronym (abbreviate) where
import Data.Char (toUpper)

lettersAndWhitespace :: String
lettersAndWhitespace = ['a'..'z'] ++ ['A'..'Z'] ++ [' ']

clean :: String -> String
clean [] = []
clean (x:xs)
    | x `elem` lettersAndWhitespace = x : clean xs
    | otherwise = clean xs

split :: String -> Char -> [String]
split str delimiter
  | null a && null b = []                        
  | null a && not (null b) = split next delimiter
  | otherwise = a : split next delimiter
  where
    (a, b) = break (== delimiter) str
    next
        | null b = b
        | otherwise = tail b

abbreviate :: String -> String
abbreviate xs = [toUpper $ head word | word <- split (clean xs) ' ', fromIntegral (length word) > 0]