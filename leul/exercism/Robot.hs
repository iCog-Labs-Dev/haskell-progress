-- https://exercism.org/tracks/haskell/exercises/robot-simulator/
module Robot
    ( Bearing(East,North,South,West)
    , bearing
    , coordinates
    , mkRobot
    , move
    ) where

import Data.List ( elemIndex )

data Bearing = North
             | East
             | South
             | West
             deriving (Eq, Show)

data Robot =  Robot {
    bearing :: Bearing,
    coordinates :: (Integer, Integer)
}

fromJust :: Maybe a -> a
fromJust (Just a) = a


mkRobot :: Bearing -> (Integer, Integer) -> Robot
mkRobot direction coordinates = Robot {bearing=direction, coordinates=coordinates}


advance :: Bearing -> (Integer, Integer) -> (Integer, Integer)
advance bearing coord = case bearing of
                            North -> (fst coord, snd coord + 1)
                            South -> (fst coord, snd coord - 1)
                            East -> (fst coord + 1, snd coord)
                            West -> (fst coord - 1, snd coord)


turn :: Bearing -> Char -> Bearing
turn bearing t = case t of 
                    'R' -> bearings !! ((bearingIndex + 1) `mod` 4)
                    'L' -> bearings !! ((bearingIndex - 1) `mod` 4)
                
                where bearings = [North, East, South, West]
                      bearingIndex = fromJust $ bearing `elemIndex` bearings
    


move :: Robot -> String -> Robot
move robot [] = robot
move robot (x:xs) = move (step x) xs
                    where 
                          currentBearing = bearing robot
                          currentCoord = coordinates robot    
                          step x = case x of
                                    'R' -> Robot {bearing = turn currentBearing 'R', coordinates = currentCoord}
                                    'L' -> Robot {bearing = turn currentBearing 'L', coordinates = currentCoord}
                                    'A' -> Robot {bearing = currentBearing, coordinates = advance currentBearing currentCoord}                                            
                          

    
    
