import Data.Char qualified as Char
import Data.List qualified as List
import Data.Map qualified as Map
import Data.Maybe qualified as Maybe
import Data.Set qualified as Set
import Data.Text qualified as Text

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
sumOfMultiples' :: [Integer] -> Integer -> Integer
sumOfMultiples' factors limit = sum $ foldl (\acc x -> acc `Set.union` Set.fromList [x, 2 * x .. limit - 1]) Set.empty factors

-- Using Data.List
sumOfMultiples :: [Integer] -> Integer -> Integer
sumOfMultiples factors limit = sum $ List.nub $ concatMap (\x -> [x, x * 2 .. limit - 1]) factors

-- -- Using Data.List
-- sumOfMultiples' :: [Integer] -> Integer -> Integer
-- sumOfMultiples' factors limit = sum $ List.nub $ foldl (\acc x -> acc ++ takeWhile (< limit) (map (* x) [1 ..])) [] factors

-- Grains
-- https://exercism.org/tracks/haskell/exercises/grains

square :: Integer -> Maybe Integer
square n
  | n `elem` [1 .. 64] = Just (2 ^ (n - 1))
  | otherwise = Nothing

total :: Integer
total = foldl (\acc x -> acc + Maybe.fromMaybe (error "square error") (square x)) 0 [1 .. 64]

-- Acronym
-- https://exercism.org/tracks/haskell/exercises/acronym

abbreviate :: Text.Text -> Text.Text
abbreviate xs = foldl (\acc x -> Text.append acc (extractCaps x Text.empty False)) Text.empty splittedText
  where
    space = Text.pack " "
    dash = Text.pack "-"
    splittedText = filter (/= Text.empty) (concatMap (Text.splitOn dash) (Text.splitOn space xs))
    extractCaps :: Text.Text -> Text.Text -> Bool -> Text.Text
    extractCaps xs acc fstCap
      | tl == Text.empty && hd == ' ' = acc
      | not fstCap =
          if Char.isAlpha hd
            then extractCaps tl (Text.cons (Char.toUpper hd) acc) True
            else extractCaps tl acc False
      | otherwise = Text.append acc (checkCamel tl True Text.empty)
      where
        hd = fst $ Maybe.fromMaybe (' ', Text.empty) (Text.uncons xs)
        tl = snd $ Maybe.fromMaybe (' ', Text.empty) (Text.uncons xs)
        checkCamel xs prev acc
          | xs == Text.empty = acc
          | Char.isUpper hd =
              if not prev
                then checkCamel tl True (Text.cons hd acc)
                else checkCamel tl True acc
          | otherwise = checkCamel tl False acc
          where
            hd = fst $ Maybe.fromMaybe (' ', Text.empty) (Text.uncons xs)
            tl = snd $ Maybe.fromMaybe (' ', Text.empty) (Text.uncons xs)

-- Strain
-- https://exercism.org/tracks/haskell/exercises/strain

-- strict version

discard' :: (a -> Bool) -> [a] -> [a]
discard' p = foldl (\acc x -> if not $ p x then acc ++ [x] else acc) []

keep' :: (a -> Bool) -> [a] -> [a]
keep' p = foldl (\acc x -> if p x then acc ++ [x] else acc) []

-- lazy version
discard :: (a -> Bool) -> [a] -> [a]
discard p [] = []
discard p (x : xs)
  | p x = discard p xs
  | otherwise = x : discard p xs

keep :: (a -> Bool) -> [a] -> [a]
keep p [] = []
keep p (x : xs)
  | not $ p x = keep p xs
  | otherwise = x : keep p xs

-- Anagrams
-- https://exercism.org/tracks/haskell/exercises/anagram

anagramsFor :: String -> [String] -> [String]
anagramsFor xs [] = []
anagramsFor xs (xx : xss) =
  if isAnagram frmList toBeChecked && (frmList /= toBeChecked)
    then xx : anagramsFor xs xss
    else anagramsFor xs xss
  where
    frmList = map Char.toLower xx
    toBeChecked = map Char.toLower xs
    isAnagram :: String -> String -> Bool
    isAnagram "" xs = null xs
    isAnagram (x : xx) xs
      | x `elem` xs = isAnagram xx (List.delete x xs)
      | otherwise = False
