module SpaceAge (Planet(..), ageOn) where

data Planet = Mercury
            | Venus
            | Earth
            | Mars
            | Jupiter
            | Saturn
            | Uranus
            | Neptune

ageOn :: Planet -> Float -> Float
ageOn Mercury seconds = seconds/(86400*365.25*0.2408467)
ageOn Venus seconds = seconds/(86400*365.25*0.61519726)
ageOn Earth seconds = seconds/(86400*365.25)
ageOn Mars seconds = seconds/(86400*365.25*1.8808158)
ageOn Jupiter seconds = seconds/(86400*365.25*11.862615)
ageOn Saturn seconds = seconds/(86400*365.25*29.447498)
ageOn Uranus seconds = seconds/(86400*365.25*84.016846)
ageOn Neptune seconds = seconds/(86400*365.25*164.79132)
          
 