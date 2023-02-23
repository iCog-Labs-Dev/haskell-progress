module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
    | planet == Mercury = earthYear / 0.2408467 
    | planet == Venus = earthYear / 0.61519726
    | planet == Earth = earthYear
    | planet == Mars = earthYear / 1.8808158 
    | planet == Jupiter = earthYear / 11.862615 
    | planet == Saturn = earthYear / 29.447498 
    | planet == Uranus = earthYear / 84.016846 
    | planet == Neptune = earthYear / 164.79132 
    | otherwise = 0.00
    where earthYear = seconds/365.25/24/60/60 -- converting seconds to years on Earth
