import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Set qualified as Set

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
isPangram :: String -> Bool
isPangram text =
  foldl
    ( \acc x ->
        if not acc
          then acc
          else x `elem` map Char.toLower text
    )
    True
    ['a' .. 'z']

-- using Data.Set
isPangram' :: String -> Bool
isPangram' text = Set.size (Set.delete ' ' (Set.fromList (map Char.toLower text))) == 26

-- Bob
-- https://exercism.org/tracks/haskell/exercises/bob

responseFor :: String -> String
responseFor xs
  | all Char.isSpace xs || null xs = "Fine. Be that way!"
  | qmExists xs = handleQM xs
  | areUpper xs = "Whoa, chill out!"
  | otherwise = "Whatever."
  where
    qmExists xs = last (filter (not . Char.isSpace) xs) == '?'
    handleQM xs
      | areUpper xs = "Calm down, I know what I'm doing!"
      | otherwise = "Sure."
    areUpper xs =
      all
        Char.isUpper
        ( if filtered /= ""
            then filtered
            else xs
        )
      where
        filtered = filter (`elem` (['a' .. 'z'] ++ ['A' .. 'Z'])) xs

-- Collatz Conjecture
-- https://exercism.org/tracks/haskell/exercises/collatz-conjecture/

collatz :: Integer -> Maybe Integer
collatz n = calc n 0
  where
    calc :: Integer -> Integer -> Maybe Integer
    calc x counter
      | x == 1 = Just counter
      | x <= 0 = Nothing
      | even x = calc (x `div` 2) (counter + 1)
      | odd x = calc ((3 * x) + 1) (counter + 1)
      | otherwise = Nothing

-- Rna Transcription
-- https://exercism.org/tracks/haskell/exercises/rna-transcription

toRNA :: String -> Either Char String
toRNA xs = registerRNA xs ""
  where
    registerRNA :: String -> String -> Either Char String
    registerRNA "" stk = Right stk
    registerRNA (x : xs) stk
      | x `elem` "GCTA" = registerRNA xs (stk ++ findCompliment x : "")
      | otherwise = Left x
    findCompliment :: Char -> Char
    findCompliment s
      | s == 'G' = 'C'
      | s == 'C' = 'G'
      | s == 'T' = 'A'
      | s == 'A' = 'U'

-- Nucleotide count
-- https://exercism.org/tracks/haskell/exercises/nucleotide-count

data Nucleotide = A | C | G | T deriving (Eq, Ord, Show)

type Counter = Map.Map Nucleotide Int

nucleotideCounts :: String -> Either String Counter
nucleotideCounts xs = doCount xs (Map.fromList [(A, 0), (C, 0), (G, 0), (T, 0)])
  where
    doCount :: String -> Counter -> Either String Counter
    doCount "" counter = Right counter
    doCount (x : xs) counter
      | x == 'G' = doCount xs (Map.adjust (+ 1) G counter)
      | x == 'C' = doCount xs (Map.adjust (+ 1) C counter)
      | x == 'T' = doCount xs (Map.adjust (+ 1) T counter)
      | x == 'A' = doCount xs (Map.adjust (+ 1) A counter)
      | otherwise = Left "invalid input"

-- Sum Of Multiples
-- https://exercism.org/tracks/haskell/exercises/sum-of-multiples/

-- Using Data.Set (this one is the fastest)
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ foldl (\acc x -> acc `Set.union` Set.fromList [x, 2 * x .. limit - 1]) Set.empty factors

-- Using Data.List
-- comming soon