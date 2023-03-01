module Phone (number) where

import Data.Char (isDigit)

number :: String -> Maybe String
number cs
  | null cs = Nothing
  | otherwise =
      if validLocalNumber
        && (head cs /= '0')
        && length stripOne == 10
        && ((/= '0') . head) stripOne
        then Just stripOne
        else Nothing
  where
    validLocalNumber = head localNumbers /= '0' && head localNumbers /= '1'
    localNumbers = drop 3 stripOne
    stripOne = dropWhile (== '1') $ filter isDigit cs