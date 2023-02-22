-- solution for https://exercism.org/tracks/haskell/exercises/space-age

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
  | planet == Mercury = seconds / (0.2408467 * 31557600)
  | planet == Venus = seconds / (0.61519726 * 31557600)
  | planet == Earth = seconds / 31557600
  | planet == Mars = seconds / (1.8808158 * 31557600)
  | planet == Jupiter = seconds / (11.862615 * 31557600)
  | planet == Saturn = seconds / (29.447498 * 31557600)
  | planet == Uranus = seconds / (84.016846 * 31557600)
  | planet == Neptune = seconds / (164.79132 * 31557600)
