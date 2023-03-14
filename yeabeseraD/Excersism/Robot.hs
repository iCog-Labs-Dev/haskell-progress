module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where
import Data.List (intersperse)
import Data.Char (toUpper)

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot = Robot {getDirection :: Bearing, getPosition :: (Integer, Integer)} deriving (Show)

bearing :: Robot -> Bearing
bearing = getDirection

coordinates :: Robot -> (Integer, Integer)
coordinates  = getPosition

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot  

move :: Robot -> String -> Robot
move robot "" = robot
move robot@(Robot direction (x,y)) instructions = foldl (\newRobot x -> if x `elem` ["R", "L"] then changeDirection x newRobot else nextMove newRobot) robot instructionList
    where nextMove (Robot newDir (x,y))
            | newDir == North =Robot newDir (x,y+1)
            | newDir == East = Robot newDir (x+1, y)
            | newDir == South =Robot newDir  (x, y-1)
            | otherwise = Robot newDir (x-1, y)
          
          changeDirection "R" (Robot newDir (x',y'))
            | newDir == North = Robot East (x',y')
            | newDir == East = Robot South (x',y')
            | newDir == South = Robot West (x',y')
            | otherwise = Robot North (x',y')
          changeDirection "L" (Robot newDir (x',y'))
            | newDir == North = Robot West (x',y')
            | newDir == East = Robot North (x',y')
            | newDir == South = Robot East (x',y')
            | otherwise = Robot South (x',y')
          instructionList = moves instructions
moves :: String -> [String]
moves xs =words . intersperse ' ' $ cleanString
    where cleanString = filter (`elem` "RLA") ( map toUpper xs)
