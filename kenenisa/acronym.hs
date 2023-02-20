module Acronym (abbreviate) where
import Data.Char

low :: String
low = ['a'..'z']

up :: String
up = ['A'..'Z']

letter :: String
letter = up ++ low

forAllCaps :: Char -> [Char] -> (Char,[Char])
forAllCaps f [] = (f,[]) 
forAllCaps f (x:xs) | x `elem` " " = (f,xs)
                    | otherwise = forAllCaps f xs

strip :: [Char] -> String -> [Char] 
strip acc [x] = acc
strip acc []  = acc
strip acc (x:y:xs) | x `elem` up && y `elem` low      = strip (acc ++ [x]) xs
                   | x `elem` low && y `elem` up      = strip (acc ++ [y]) xs
                   | x `elem` " -" && y `elem` letter = strip (acc ++ [toUpper y]) xs
                   | x `elem` up && y `elem` up       = strip (acc ++ [fst allCaps]) $ snd allCaps
                   | otherwise                        = strip acc (y:xs)
                   where allCaps = forAllCaps x xs
                   
abbreviate :: String -> String
abbreviate = strip []
