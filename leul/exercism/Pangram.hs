import Data.Char (toLower)

letters = ['a'..'z'] ++ ['A'..'Z']

toLower' :: String -> String
toLower' [] = []
toLower' (x:[]) = toLower x : []
toLower' (x:xs) = toLower x : toLower' xs

onlyLetters text = [x | x <- text, x `elem` letters]

uniques :: (Eq a) => [a] -> [a]
uniques [] = []
uniques (x:[]) = [x]
uniques (x:xs)
    | x `elem` xs = uniques xs
    | otherwise = x : uniques xs


isPangram :: String -> Bool
isPangram text = (length . uniques . toLower' . onlyLetters $ text) == 26