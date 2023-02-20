module Bob (responseFor) where
import Data.Char (isSpace)

lowerAlphabet :: [Char]
lowerAlphabet = ['a' .. 'z']

upperAlphabet :: [Char]
upperAlphabet = ['A' .. 'Z']

containsUpperCase :: [Char] -> Bool
containsUpperCase [] = False 
containsUpperCase (x:xs) | x `elem` upperAlphabet = True | otherwise = containsUpperCase xs

isAllCaps :: [Char] -> Bool
isAllCaps [] = True
isAllCaps (x : xs) | x `elem` lowerAlphabet = False | otherwise = isAllCaps xs

isThereQuestionMark :: [Char] -> Int -> Bool
isThereQuestionMark _ 0 = False
isThereQuestionMark xs len | '?' == xs !! (len - 1) = True | isSpace (xs !! (len - 1)) = isThereQuestionMark xs (len - 1) | otherwise = False

responseText :: Bool -> Bool -> Bool -> Bool -> String
responseText question upperCased allCaps saidNothing 
  | saidNothing = "Fine. Be that way!"
  | upperCased && allCaps = if question then "Calm down, I know what I'm doing!" else "Whoa, chill out!"
  | question = "Sure."
  | otherwise = "Whatever." 

responseFor :: String -> String
responseFor xs = let question = isThereQuestionMark xs (length xs)
                     upperCased = containsUpperCase xs
                     allCaps = isAllCaps xs
                     saidNothing = null xs || all isSpace xs
                in responseText question upperCased allCaps saidNothing
