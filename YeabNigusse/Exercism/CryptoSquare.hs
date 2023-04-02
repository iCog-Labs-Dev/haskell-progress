module CryptoSquare (encode) where
import Data.Char ( isAlpha, toLower, isDigit)
import Data.List ( transpose )
encode :: String -> String
encode xs = transposit (toString (onlyAlpha xs) (length (onlyAlpha xs)))

onlyAlpha :: String -> String
onlyAlpha str = map toLower (filter (\x -> isAlpha x || isDigit x) str) 



toString :: String -> Int -> [String]
toString [] _ = []
toString str len = let r = floor (sqrt (fromIntegral len))
                       c = if fromIntegral (floor (sqrt (fromIntegral len))) == sqrt (fromIntegral len) then r else r+1
                       str' = str ++  replicate (abs ((r*c) - len)) ' '
                   in take c str' : toString (drop c str) len
transposit :: [String] -> String
transposit =  unwords . transpose 