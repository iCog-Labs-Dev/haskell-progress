module Isogram (isIsogram) where
import Data.Char ( isAlpha , toLower)
import Data.List ( nub )
isIsogram :: String -> Bool
isIsogram str = nub (filter isAlpha (map toLower str)) == filter isAlpha (map toLower str)
