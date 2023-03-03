module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int deriving (Eq)

instance Show Clock where
  show (Clock hour minute) = showClock
    where
      showClock = showHour ++ ":" ++ showMinute
      showHour = if hour < 10 then "0" ++ show hour else show hour
      showMinute = if minute < 10 then "0" ++ show minute else show minute

fromHourMin :: Int -> Int -> Clock
fromHourMin hour minute = Clock calcHour (minute `mod` 60)
  where
    calcHour = (hour + (minute `div` 60)) `mod` 24

toString :: Clock -> String
toString = show

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour minute (Clock h m) = fromHourMin (hour + h) (minute + m)
