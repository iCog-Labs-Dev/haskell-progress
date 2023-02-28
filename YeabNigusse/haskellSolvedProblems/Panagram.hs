module Pangram (isPangram) where
import Data.Char(toLower)
isPangram :: String -> Bool
isPangram text = allFound alphabet (toLowerCase text)
    

isFound :: Char -> String -> Bool
isFound x text = x `elem` text

toLowerCase :: String -> String
toLowerCase = map toLower  
 
alphabet :: [Char]
alphabet = ['a'..'z']

allFound :: String -> String-> Bool
allFound "" _ = True
allFound (x:xs) text = if isFound x text then allFound xs text else False

