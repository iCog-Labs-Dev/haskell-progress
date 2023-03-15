-- https://exercism.org/tracks/haskell/exercises/word-count/

module WordCount (wordCount) where

import Data.Char (toLower)

allowed :: String
allowed = '\'' : ['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']


split :: String -> [String]
split xs = map cleanQuotation $ words [if x `elem` allowed then toLower x else ' ' | x <- xs]

cleanQuotation :: String -> String
cleanQuotation [] = []
cleanQuotation (x:xs)
    | x == '\'' && head (reverse xs) == '\'' = init xs
    | otherwise = x:xs

getCount :: (String, Int) -> Int
getCount (_, c)= c


count :: [String] -> [(String, Int)]
count [] = []
count (x:xs)
    | length xCount == 0 = (x, 1):totalCount
    | otherwise = (x, (getCount (head xCount) + 1)) : (filter (/=(x, (getCount (head xCount)))) totalCount)
    where xCount = filter (\(s, _) -> s==x) totalCount
          totalCount = count xs


wordCount :: String -> [(String, Int)]
wordCount = count . split
