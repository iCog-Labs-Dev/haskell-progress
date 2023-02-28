module Phone (number) where

number :: String -> Maybe String
number xs 
    | len == 11 && firstOne = checkAreaCode $ tail num
    | len == 10 && not firstOne = checkAreaCode num
    | otherwise = Nothing
    where num = [x | x <- xs, x `elem` ['0'..'9']]
          len = length num
          firstOne = head num == '1'
          checkAreaCode n = if all (`elem` ['2'..'9']) [n !! 0, n !! 3]
                            then Just n 
                            else Nothing 

