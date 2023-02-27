module Acronym (abbreviate) where
import Data.Char

abbreviate :: String -> String
abbreviate xs = map toUpper $ map (head) wrds
    where wrds = words $ map (separators) $ (camelCases ' ' "" xs)

camelCases :: Char -> String -> String -> [Char]
camelCases _ camel [] = camel
camelCases prevChr camel (curChr:str) = 
        if (curChr `elem` ['A'..'Z'] && prevChr `elem` ['a'..'z'])
        then camelCases curChr (camel ++ [' ',curChr]) str
        else camelCases curChr (camel ++ [curChr]) str

separators :: Char -> Char
separators chr
    | chr == '-' = ' '
    | chr == '_' = ' '
    | otherwise = chr
