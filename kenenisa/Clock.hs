module Clock (addDelta, fromHourMin, toString) where

data Clock = Clock Int Int
  deriving Eq


fixTime :: Clock -> Clock
fixTime (Clock hour min) = Clock calcHour calcMin
                where 
                      modMin = min `mod` 60
                      calcHour = (hour + (min `div` 60)) `mod` 24
                      calcMin =  min `mod` 60
                      
fromHourMin :: Int -> Int -> Clock
fromHourMin hour min = fixTime (Clock hour min)


toString :: Clock -> String
toString (Clock hour min) = paddedHour ++ ":" ++ paddedMin
                          where paddedHour |  hour < 10 = '0' : show hour | otherwise = show hour
                                paddedMin | min < 10 = '0': show min | otherwise = show min


addDelta :: Int -> Int -> Clock -> Clock
addDelta hour min (Clock baseHour baseMin) = fixTime (Clock (baseHour+hour) (baseMin+min))
