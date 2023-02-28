module Bob (responseFor) where
import Data.Char (isUpper, isLetter, isSpace)

responseFor :: String -> String
responseFor xs
  | all isSpace xs = "Fine. Be that way!"
  | (all isUpper text) && text /= "" && (last textWithNoWhiteSpace == '?') = "Calm down, I know what I'm doing!"
  | (all isUpper text) && text /= ""  = "Whoa, chill out!"
  | last textWithNoWhiteSpace == '?' = "Sure."
  | otherwise = "Whatever."
  where text = [ a | a <- xs , a `elem` ['a'..'z'] || a `elem` ['A'..'Z']]
        textWithNoWhiteSpace = [ a | a <- xs, a /= ' ']