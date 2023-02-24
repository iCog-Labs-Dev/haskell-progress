module Bob (responseFor) where
import Data.Char

responseFor :: String -> String
responseFor xsSp
    | all isSpace xs = "Fine. Be that way!"
    | xsUp && last xs == '?' = "Calm down, I know what I'm doing!"
    | xsUp = "Whoa, chill out!"
    | last xs == '?' = "Sure." 
    | otherwise = "Whatever."
    where xs = filter (/=' ') xsSp
          xsUp = xs == (map toUpper xs) && any (`elem` ['A'..'Z']) (map toUpper xs)

