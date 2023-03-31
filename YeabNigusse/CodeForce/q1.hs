import Data.Char
import Data.List
main = 
    interact $ purelist . concat . words

purelist xs = show (length (nub (filter isAlpha xs)))