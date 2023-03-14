module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving (Eq,Show)

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = let minit = min `mod` 60
                           hr = (hour + min `div` 60) `mod` 24
                           in Clock hr minit

toString :: Clock -> String
toString (Clock hr min)
                 | hr < 10 && min < 10 = "0" ++ show hr ++ ":" ++ "0" ++ show min
                 | hr > 9 && min < 10 = show hr ++ ":" ++ "0" ++ show min
                 | hr < 10 && min > 9 = "0" ++ show hr ++ ":" ++ show min
                 | otherwise = show hr ++ ":" ++ show min

addDelta :: Int -> Int -> Clock -> Clock
addDelta hd md (Clock h m) = fromHourMin (h + hd) (m + md)
                              
