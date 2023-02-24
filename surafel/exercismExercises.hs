import Data.Char qualified as Char
import Data.Set qualified as Set
import Data.String qualified as Set

-- Leap
-- https://exercism.org/tracks/haskell/exercises/leap
isLeapYear :: Integer -> Bool
isLeapYear year
  | isDivBy4 = not isDivBy100 || isDivBy400
  | otherwise = False
  where
    isDivBy4 = (year `mod` 4) == 0
    isDivBy400 = (year `mod` 400) == 0
    isDivBy100 = (year `mod` 100) == 0

-- SpaceAge
-- https://exercism.org/tracks/haskell/exercises/space-age
data Planet
  = Mercury
  | Venus
  | Earth
  | Mars
  | Jupiter
  | Saturn
  | Uranus
  | Neptune
  deriving (Eq)

ageOn :: Planet -> Float -> Float
ageOn planet seconds
  | planet == Mercury = years / 0.2408467
  | planet == Venus = years / 0.61519726
  | planet == Earth = years / 1.0
  | planet == Mars = years / 1.8808158
  | planet == Jupiter = years / 11.862615
  | planet == Saturn = years / 29.447498
  | planet == Uranus = years / 84.016846
  | planet == Neptune = years / 164.79132
  where
    years = seconds / (60 * 60 * 24 * 365.25) -- seconds * minutes * days * years

-- Pangram
-- https://exercism.org/tracks/haskell/exercises/pangram

-- without using Data.Set
-- working on it ...

-- using Data.Set
isPangram :: String -> Bool
isPangram text = Set.size (Set.delete ' ' (Set.fromList (map Char.toLower text))) == 26
