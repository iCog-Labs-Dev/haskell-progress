module Phone (number) where
import Data.Char ( isDigit )
number :: String -> Maybe String
number xs 
        | head (toPureString xs) /= '1' && length (toPureString xs) == 11 = Nothing
        | length (toPureString xs) == 11 && (toPureString xs) !! 1 == '1'  = Nothing
        | length (toPureString xs) == 11 && (toPureString xs) !! 1 == '0'  = Nothing
        | length (toPureString xs) == 11 && (toPureString xs) !! 4 == '0'  = Nothing
        | length (toPureString xs) == 11 && (toPureString xs) !! 4 == '1'  = Nothing
        | (toPureString xs) !! 3 == '0'  = Nothing
        | length (toPureString xs) == 11 && (toPureString xs) !! 0 == '1'  = Just (tail (toPureString xs))
        | length (toPureString xs) < 10 || length (toPureString xs) > 10  = Nothing
        | head (toPureString xs) /= '1' && length (toPureString xs) == 11 = Just (toPureString xs)
        | length (toPureString xs) == 10 && head (toPureString xs) == '0'  = Nothing
        | (toPureString xs) !! 0 == '1'  = Nothing
        | otherwise = Just (toPureString xs)


toPureString :: String -> String
toPureString xs = [x | x <- xs, isDigit x]
