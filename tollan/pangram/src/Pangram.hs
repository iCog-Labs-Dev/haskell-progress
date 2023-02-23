module Pangram (isPangram) where
import Data.Char

isPangram :: String -> Bool
isPangram text = all (`elem` map toUpper text) ['A'..'Z']
-- isPangram text = (length $ Set.filter (`elem` ['A'..'Z']) $ Set.fromList $ map toUpper text) == 26