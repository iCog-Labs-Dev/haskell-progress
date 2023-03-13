-- https://exercism.org/tracks/haskell/exercises/robot-simulator/


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

data Robot =  Robot {
    bearing :: Bearing,
    coordinates :: (Integer, Integer)
}


mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot {bearing=direction, coordinates=coordinates}

move :: Robot -> String -> Robot
move robot [] = robot
-- move robot (x:xs) = case x of
    
    
