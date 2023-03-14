-- https://exercism.org/tracks/haskell/exercises/clock

module Clock (addDelta, fromHourMin, toString) where
import Text.Printf (printf)


type Hour = Int
type Minute = Int

data Clock = Clock Hour Minute 
    deriving Eq  


fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock newHour newMin
    where newHour = (hour + carry) `mod` 24
          newMin = min `mod` 60
          carry = floor (fromIntegral min / 60)

toString :: Clock -> String
toString (Clock hour min) = printf "%02d:%02d" hour min

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock h m) = Clock newHour newMin
    where newHour = (hour + h + carry) `mod` 24
          newMin = (m + min) `mod` 60
          carry = floor (fromIntegral (m + min) / 60)
