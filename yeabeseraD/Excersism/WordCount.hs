module WordCount (wordCount) where
import Data.Char (isAlphaNum, toLower)
import Data.List (sort, group)

wordCount :: String -> [(String, Int)]
wordCount xs = removeEmptyStr $ countWords $ map cleanString (words $ expandCrammed xs)

cleanString :: String -> String
cleanString xs =removeCoutes $ map toLower (filter (\x -> isAlphaNum x || x == '\'') xs)
    where removeCoutes ys 
            | length ys > 2  = if head ys == '\'' && last ys == '\'' then (tail . init) ys else ys
            | otherwise = ys

countWords :: [String] -> [(String, Int)]
countWords xs = map (\str@(x:ys) -> (x, length str)) (groupWords xs)
    where groupWords = group . sort

expandCrammed :: String -> String
expandCrammed = map (\x -> if x == ',' then ' ' else x)

removeEmptyStr :: [(String, Int)] -> [(String, Int)]
removeEmptyStr = filter (\(str, _) -> str /= "" )