-- https://exercism.org/tracks/haskell/exercises/queen-attack/

module Queens (boardString, canAttack) where
import Data.List (intercalate)


trim :: String -> String
trim s = reverse $ dropWhile (==' ') (reverse leftTrim)
    where leftTrim = dropWhile (==' ') s



buildRow :: [(Int, Char)] -> String
buildRow [] = unwords $ replicate 8 "_"
buildRow [(x, charX)]  = trim $ unwords (replicate x "_") ++ [' ', charX, ' '] ++ unwords (replicate (8 - x - 1) "_")
buildRow [(x, charX), (y, charY)] = trim $ unwords (replicate x "_") ++ [' ', charX, ' '] ++ unwords (replicate (y - x - 1) "_") ++ [charY] ++ unwords (replicate (8 - y - 1) "_")


boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = intercalate "\n" (replicate 8 (buildRow [])) ++ "\n"
boardString Nothing (Just (c, d)) = intercalate "\n" [
            if r == c then buildRow [(d, 'B')]             
            else buildRow [] | r <- [0..7]
        ] ++ "\n"
boardString (Just (c, d)) Nothing = intercalate "\n" [
            if r == c then buildRow [(d, 'W')]             
            else buildRow [] | r <- [0..7]
        ] ++ "\n"
boardString (Just (a, b)) (Just (c, d))
    | a == c && b < d = intercalate "\n" [
        if r == a then buildRow [(b, 'W'), (d, 'B')] else buildRow [] | r <- [0..7]
        ] ++ "\n"
    
    | a == c && b > d = intercalate "\n" [
        if r == a then buildRow [(d, 'B'), (b, 'W')] else buildRow [] | r <- [0..7]
        ] ++ "\n"
    
    
    | otherwise = intercalate "\n" [
        if r == a then buildRow [(b, 'W')] 
        else if r == c then buildRow [(d, 'B')] 
        else buildRow [] | r <- [0..7]
        ] ++ "\n"



canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (ar, ac) (br, bc)
    | ar == br || ac == bc = True
    | abs(ar - br) == abs(ac - bc) = True
    | otherwise = False

