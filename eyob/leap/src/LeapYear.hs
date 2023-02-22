module LeapYear (isLeapYear) where

isLeapYear :: Integer -> Bool
-- isLeapYear year = error "You need to implement this function."
isLeapYear year
  | isDivisibleBy 400 = True
  | isDivisibleBy 100 = False
  | isDivisibleBy 4 = True
  | otherwise = False
  where isDivisibleBy d = year `mod` d == 0
      