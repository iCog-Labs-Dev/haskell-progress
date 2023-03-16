module Queens (boardString, canAttack) where

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> [String]
boardString Nothing Nothing = [""]
boardString (Just (a, b)) (Just (c, d)) = [""]
             

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack (a, b) (c, d) 
           | a == c = True
           | b == d = True
           | c-a == d-b = True
           | otherwise = False
