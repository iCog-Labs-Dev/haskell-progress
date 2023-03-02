filterRepeated :: String -> Bool
filterRepeated [] = True
filterRepeated (x:xs) | x `elem` xs = False
                      | otherwise = filterRepeated xs
main :: IO ()
main = do
    n <- getLine
    students <- getLine
    banned <- getLine
    print $ length $ filter filterRepeated (filter (`notElem` words banned) (words students))
