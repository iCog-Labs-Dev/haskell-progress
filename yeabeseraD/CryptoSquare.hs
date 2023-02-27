module CryptoSquare (encode) where
import Data.Char (isAlphaNum, toLower)
import Data.String (IsString(fromString))
import Data.List

encode :: String -> String
encode xs = encodedMessage
    where text = normalizeText xs
          (r,c) = decideRowAndCol (length text)
          filledText = fillSpace text (r, c)
          matrix = init $ subStringText filledText (r,c)
          encodedMessage = getEncMessage matrix

normalizeText :: String -> String
normalizeText xs = map toLower (filter isAlphaNum xs)

decideRowAndCol :: (Integral a) => a -> (a, a)
decideRowAndCol n = (round x, round x)
    where x = sqrt (fromIntegral n)

fillSpace :: String->(Int, Int)->String
fillSpace xs (r, c)
    | length xs == r * c = xs
    | otherwise = fillSpace (xs ++ [' ']) (r, c)

subStringText :: String->(Int, Int)->[String]
subStringText "" (_,_) = [[]]
subStringText xs (r, c) = let (x, y) = splitAt c xs
    in x : subStringText y (r, c)

getEncMessage :: [String] -> String
getEncMessage xs
    | all null xs = ""
    | all (\x -> length x == 1) xs = map head xs
    | otherwise = map head xs ++ " " ++ getEncMessage (map tail xs)