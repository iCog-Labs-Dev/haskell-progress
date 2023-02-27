module Acronym (abbreviate) where
import Data.List (words)

abbreviate :: String -> String
abbreviate xs = map head (words xs)
