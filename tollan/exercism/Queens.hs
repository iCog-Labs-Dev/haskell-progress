module Queens (boardString, canAttack) where
import Data.List (intersperse)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> [Char]
boardString Nothing Nothing = board (-1,-1) (-1,-1)
boardString (Just (wX, wY)) Nothing = board (wX,wY) (-1,-1)
boardString Nothing (Just (bX, bY)) = board (-1,-1) (bX,bY)
boardString (Just (wX, wY)) (Just (bX, bY)) = board (wX,wY) (bX,bY)

emptyLine :: [Char]
emptyLine = map (\x -> '_') ['0'..'7']

line :: Char -> Char -> [Char]
line char pos = map (\x -> if (x /= pos) then '_' else char) ['0'..'7']

board :: (Int, Int) -> (Int, Int) -> [Char]
board (wX,wY) (bX,bY) =  unlines $ map (intersperse ' ') $ map (\x -> if x == wX 
                                then line 'W' (head $ show wY) 
                                else if x == bX
                                      then line 'B' (head $ show bY)
                                        else emptyLine
                                        ) [0..7]


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack queenA queenB
    | fstA == fstB = True
    | sndA == sndB = True
    | (abs (fstA - fstB)) == (abs (sndA - sndB)) = True
    | otherwise = False
    where fstA = fst queenA
          fstB = fst queenB
          sndA = snd queenA
          sndB = snd queenB