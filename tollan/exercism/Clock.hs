module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock {
                    hour :: Integer, 
                    minute :: Integer
                  }
  deriving (Eq, Show)

fromHourMin :: Integer -> Integer -> Clock
fromHourMin hour min = Clock ((hour + (min `div` 60)) `mod` 24) (min `mod` 60)

toString :: Clock -> String
toString clock = (if length hr==1 then "0"++hr else hr) ++ ":" ++ (if length min==1 then "0"++min else min)
    where hr = show $ hour clock
          min = show $ minute clock

addDelta :: Integer -> Integer -> Clock -> Clock
addDelta hr min clock = Clock (((hrPrev + hr) + ((minPrev + min) `div` 60)) `mod` 24) ((minPrev + min) `mod` 60)
    where hrPrev = hour clock
          minPrev = minute clock
