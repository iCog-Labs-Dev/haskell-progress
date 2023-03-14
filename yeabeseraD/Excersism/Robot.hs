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
             deriving (Eq, Show,Enum)

data Robot = Robot {getDirection :: Bearing, getPosition :: (Integer, Integer)} deriving (Show)

bearing :: Robot -> Bearing
bearing = getDirection

coordinates :: Robot -> (Integer, Integer)
coordinates  = getPosition

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot = Robot  

move :: Robot -> String -> Robot
move robot "" = robot
move robot@(Robot direction (x,y)) instructions = foldl (flip chooseFunction) robot instructionList
    where instructionList = moves instructions
          chooseFunction "A" = advanceRobot
          chooseFunction xs = changeDirection xs

advanceRobot :: Robot -> Robot
advanceRobot (Robot newDir (x,y)) = 
    case newDir of
    North -> Robot newDir (x,y+1)
    East -> Robot newDir (x+1, y)
    South -> Robot newDir  (x, y-1)
    West -> Robot newDir (x-1, y)

changeDirection:: String -> Robot -> Robot
changeDirection "R" (Robot newDir position) = Robot (rotateRight newDir) position
changeDirection "L" (Robot newDir position) = Robot (rotateLeft newDir) position


rotateRight :: Bearing -> Bearing
rotateRight West = North
rotateRight dir = succ dir

rotateLeft :: Bearing -> Bearing
rotateLeft North = West
rotateLeft dir = pred dir

moves :: String -> [String]
moves xs =words . intersperse ' ' $ cleanString
    where cleanString = filter (`elem` "RLA") ( map toUpper xs)
