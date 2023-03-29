module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = unlines $ map bordField [0 .. 7]
                             where bordField n = unwords $ map bordLine [0 .. 7]
                                    where bordLine m
                                           | white == Just (n, m) = "W"
                                           | black == Just (n, m) = "B"
                                           | otherwise = "_"
             

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (a, b) (c, d) 
           | a == c = True
           | b == d = True
           | abs (c-a) == abs (d-b) = True
           | otherwise = False
