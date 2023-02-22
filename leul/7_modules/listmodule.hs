import Data.List (intercalate, intersperse, transpose, find)

who :: [Char]
who = intersperse '.' "WHO"

joinBySpace :: [String] -> String
joinBySpace = intercalate " "

transposed :: [[Integer]]
transposed = transpose [[1, 2, 3], [4, 5, 6], [7, 8, 9]]

squares :: [Integer]
squares = iterate (* 2) 1

trimLeft :: String -> String
trimLeft = dropWhile (== ' ')

firstWord :: String -> String
firstWord = takeWhile (/= ' ') . trimLeft

split :: (Eq a) => [a] -> a -> [[a]]
split list separator
  | null a && null b = []                           -- finished
  | null a && not (null b) = split next separator   -- separator at the beginning
  | otherwise = a : split next separator            -- normal case
  where
    (a, b) = break (== separator) list
    next
        | null b = b
        | otherwise = tail b