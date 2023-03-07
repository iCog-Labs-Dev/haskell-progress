module Clock (addDelta, fromHourMin, toString) where

type Hour = String
type Mins = String

data Clock = Clock Hour Mins deriving Eq

fromHourMin :: Int -> Int -> Clock
fromHourMin hour min
  | validateInput hour min = let (hour', min') = getHourAndMin hour min in Clock (show hour') (show min')
  | otherwise = error "Negative input detected"

  where validateInput hour min = hour >= 0 && min >=0
        getHourAndMin hour min = let sec = (hour * 60 + min) * 60; hour' = sec `div` 3600; min' = (sec `mod` 3600) `div` 60
                                 in (hour', min')                           

toString :: Clock -> String
toString clock = 

addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min clock = error "You need to implement this function."
