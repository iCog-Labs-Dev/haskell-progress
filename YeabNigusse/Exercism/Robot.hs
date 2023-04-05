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

data Robot = Robot Bearing (Integer, Integer)

bearing :: Robot -> Bearing
bearing (Robot bearing _) = bearing

coordinates :: Robot -> (Integer, Integer)
coordinates (Robot _ position) = position

mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot direction coordinates

move :: Robot -> String -> Robot
move robot [] = robot
move robot (x:xs)
    | x == 'R'  = move (turnRight robot) xs
    | x == 'L'  = move (turnLeft robot) xs
    | x == 'A'  = move (advance robot) xs
    | otherwise = error "Invalid instruction"
                        
turnRight :: Robot -> Robot
turnRight (Robot direction position)
                 | direction == North = Robot East position
                 | direction == East = Robot South position
                 | direction == South = Robot West position
                 | direction == West = Robot North position
turnLeft :: Robot -> Robot
turnLeft (Robot direction position)
                | direction == North = Robot West position
                | direction == East = Robot North position
                | direction == South = Robot East position
                | direction == West = Robot South position
advance :: Robot -> Robot
advance (Robot direction (x, y))
                  | direction == North = Robot direction (x, y + 1)
                  | direction == East = Robot direction (x+1, y)
                  | direction == West = Robot direction (x-1, y)
                  | direction == South = Robot direction (x, y - 1)
                 
                    
