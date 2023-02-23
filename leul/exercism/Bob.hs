-- https://exercism.org/tracks/haskell/exercises/bob/

module Bob (responseFor) where

lowerCases :: [Char]
lowerCases = ['a'..'z']

whitespace :: [Char]
whitespace = " \t\n\r"

letters :: [Char]
letters = lowerCases ++ ['A'..'Z']


isQuestion :: String -> Bool
isQuestion [] = False
isQuestion str = (last str) == '?'


isAllCaps :: String -> Bool
isAllCaps [] = True
isAllCaps (x:xs)    
    | x `elem` lowerCases = False
    | otherwise = True && isAllCaps xs


trim :: String -> String
trim [] = []
trim (x:xs)
    | x `elem` whitespace = trim xs
    | otherwise = x : trim xs


isSilence :: String -> Bool
isSilence xs = 0 == (length . trim) xs


hasLetter :: String -> Bool
hasLetter [] = False
hasLetter (x:xs)
    | (x `elem` letters) = True
    | otherwise = hasLetter xs


responseFor :: String -> String
responseFor str
    | isSilence xs = "Fine. Be that way!"
    | hasLetter xs && isQuestion xs && isAllCaps xs = "Calm down, I know what I'm doing!"
    | isQuestion xs = "Sure."    
    | hasLetter xs && isAllCaps xs = "Whoa, chill out!"
    | otherwise = "Whatever."
    where xs = trim str

