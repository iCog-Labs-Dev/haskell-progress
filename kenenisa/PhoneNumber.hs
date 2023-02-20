module Phone (number) where

digits :: [Char]
digits = ['0'..'9']

cleanUp :: [Char] -> [Char]
cleanUp [] = []
cleanUp (x:xs) | x `elem` digits = x : cleanUp xs
               | otherwise = cleanUp xs
checkValid :: [Char] -> Bool
checkValid phone@(a:b:c:d:xs) | length phone /= 10 = False
                              | a == '0' || a == '1' || d == '0' || d == '1' = False
                              | otherwise = True

removeOne :: [Char] -> [Char]
removeOne (x:xs) | x == '1' = xs
                 | otherwise = x:xs

number :: String -> Maybe String
number xs  | checkValid cleanedUp = Just cleanedUp
           | otherwise = Nothing
            where cleanedUp = removeOne (cleanUp xs)
