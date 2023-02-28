module Phone (number) where
import Data.Char (isAlphaNum)

number :: String -> Maybe String
number xs = numIsValid num
  where num = [a | a <- xs, isAlphaNum a]
        numIsValid xs 
          | length xs == 10 && head xs `elem` ['2','3'..'9'] && xs !! 3 `notElem` ['1', '0'] =Just xs
          | length xs == 11 && head xs == '1' = numIsValid $ tail xs    
          | otherwise = Nothing