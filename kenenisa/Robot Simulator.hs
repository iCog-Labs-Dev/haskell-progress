module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)
data Coordinate = Coordinate (Integer,Integer)

data Robot = Robot Bearing Coordinate

bearing :: Robot -> Bearing
bearing (Robot b _) = b

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ (Coordinate c)) = c

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction (Coordinate coordinates)

advance :: Bearing -> Coordinate -> Coordinate
advance North (Coordinate (x,y))= Coordinate (x,y+1)
advance East (Coordinate (x,y))= Coordinate (x+1,y)
advance South (Coordinate (x,y))= Coordinate (x,y-1)
advance West (Coordinate (x,y))= Coordinate (x-1,y)

changeBearing :: Char -> Bearing -> Bearing
changeBearing 'L' North = West
changeBearing 'L' West = South
changeBearing 'L' South = East
changeBearing 'L' East = North

changeBearing 'R' North = East
changeBearing 'R' West = North
changeBearing 'R' South = West
changeBearing 'R' East = South


move :: Robot -> String -> Robot
move robot [] = robot
move (Robot bearing c) (i:instructions) | i == 'L' || i == 'R' = move (Robot (changeBearing i bearing) c) instructions
                                        | otherwise = move (Robot bearing (advance bearing c)) instructions
