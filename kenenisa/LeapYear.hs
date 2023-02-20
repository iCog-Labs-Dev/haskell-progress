module LeapYear (isLeapYear) where
  
isLeapYear :: Integer -> Bool
isLeapYear year =
  if year `mod` 100 == 0
    then year `mod` 400 == 0
    else year `mod` 4 == 0
