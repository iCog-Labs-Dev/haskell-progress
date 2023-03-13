module Clock (addDelta, fromHourMin, toString) where
import Data.Char (intToDigit)

type Hour = String
type Mins = String

data Clock = Clock Hour Mins deriving (Eq, Show)

fixTime ::  Int -> Int -> (Int, Int)
fixTime hour min = (hour', min')
  where min' = min `mod` 60
        hour' = (min `div` 60 + hour) `mod` 24

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = Clock (show fixedHour) (show fixedMin)
  where (fixedHour, fixedMin) = fixTime hour min

toString :: Clock -> String
toString (Clock hour min) = paddedHour ++ ":" ++ paddedMin
  where paddedHour = if length hour == 1 then '0':hour else hour
        paddedMin = if length min == 1 then '0':min else min

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock initialHour initialMin) = fromHourMin (read initialHour + hour) (read initialMin + min)
