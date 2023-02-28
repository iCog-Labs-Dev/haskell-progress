module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
isLeapYear year 
           | year `mod` 4 /= 0 = False -- I dont understand why the reverse is not true
           | year `mod` 400 == 0 = True
           | year `mod` 100 /= 0 = True
           | otherwise =  False
