module Scrabble (scoreLetter, scoreWord) where

import Data.Char (toUpper)

letterScores :: [(String, Integer)]
letterScores = [("AEIOULNRST",1),("DG",2),("BCMP",3),("FHVWY",4),("K",5),("JX",8),("QZ",10)]


scoreLetter :: Char -> Integer
scoreLetter letter = result letterScores
      where result [] = 0
            result (x:xs) | toUpper letter `elem` fst x = snd x
                          | otherwise = result xs

scoreWord :: String -> Integer
scoreWord = foldr ((+) . scoreLetter) 0
