module Queens (boardString, canAttack) where
import Data.List (intersperse)

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString white black = withBlackQueen
    where emptyBoard =  unlines . replicate 8 $ intersperse ' ' $ replicate 8 '_'
          withWhiteQueen = drawQueen white 'W' emptyBoard
          withBlackQueen = drawQueen black 'B' withWhiteQueen

canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack white black = or [
                                        leftAttack white black,
                                        rightAttack white black,
                                        topAttack white black,
                                        bottomAttack white black,
                                        topLeftAttack white black,
                                        topRightAttack white black,
                                        bottomLeftAttack white black,
                                        bottomRightAttack white black
                                    ]

leftAttack :: (Int, Int) -> (Int, Int) -> Bool
leftAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || leftAttack (x,y-1) (x',y')
    | otherwise = False

rightAttack :: (Int, Int) -> (Int, Int) -> Bool
rightAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || rightAttack (x,y+1) (x',y')
    | otherwise = False

topAttack :: (Int, Int) -> (Int, Int) -> Bool
topAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || topAttack (x-1,y) (x',y')
    | otherwise = False

bottomAttack :: (Int, Int) -> (Int, Int) -> Bool
bottomAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || bottomAttack (x+1,y) (x',y')
    | otherwise = False

topLeftAttack :: (Int, Int) -> (Int, Int) -> Bool
topLeftAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || topLeftAttack (x-1,y-1) (x',y')
    | otherwise = False

topRightAttack :: (Int, Int) -> (Int, Int) -> Bool
topRightAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || topRightAttack (x-1,y+1) (x',y')
    | otherwise = False

bottomLeftAttack :: (Int, Int) -> (Int, Int) -> Bool
bottomLeftAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || bottomLeftAttack (x+1,y-1) (x',y')
    | otherwise = False

bottomRightAttack :: (Int, Int) -> (Int, Int) -> Bool
bottomRightAttack (x,y) (x', y')
    | inBoard (x,y) = ((x', y') == (x,y)) || bottomRightAttack (x+1,y+1) (x',y')
    | otherwise = False


inBoard :: (Int, Int) -> Bool
inBoard (x,y) = (x >= 0 && y >= 0) && (x<8 && y <8)

addTuple ::(Num a) => (a, a) -> (a, a) -> (a, a)
addTuple (x1,y1) (x2,y2) = (x1+y1, x2+y2)

drawQueen :: Maybe (Int, Int)-> Char ->String -> String
drawQueen Nothing _ board = board
drawQueen (Just (r,c)) queen board = unlines updatedBoard
    where board' = lines board
          currentRow = board' !! r
          removedPadRow = concat . words $ currentRow
          updatedRow = take c removedPadRow ++ [queen] ++ drop (c+1) removedPadRow
          paddedRow = intersperse ' ' updatedRow
          updatedBoard = take r board' ++ [paddedRow] ++ drop (r+1) board'