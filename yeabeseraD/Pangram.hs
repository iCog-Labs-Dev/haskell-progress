module Pangram (isPangram) where
import Data.Char (toLower)

isPangram :: String -> Bool
isPangram text = let alphabet = ['a'..'z']; lowertext = toLowerCase text; 
                 in foundIn alphabet lowertext

toLowerCase :: String -> String
toLowerCase text = map toLower text 
foundIn :: String -> String -> Bool
foundIn "" _ = True
foundIn (x:xs) text
    | x `elem` text = foundIn xs text 
    | otherwise = False