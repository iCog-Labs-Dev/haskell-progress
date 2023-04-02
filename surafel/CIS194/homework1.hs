validate :: Integer -> Bool
validate = isValid
  where
    getPred = sumDigits . doubleEveryOther . toDigits
    isValid x = getPred x `mod` 10 == 0 || False

toDigits :: Integer -> [Integer]
toDigits 0 = []
toDigits ints
  | ints < 0 = []
  | otherwise = snd (extractDigit ints) : toDigits (fst (extractDigit ints))

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ints =
  filter
    (/= (-1))
    [if even count then 2 * x else x | (x, count) <- zip ints [1 ..]]

sumDigits :: [Integer] -> Integer
sumDigits = foldl doSum 0
  where
    doSum :: Integer -> Integer -> Integer
    doSum acc 0 = acc
    doSum acc x = doSum (acc + snd (extractDigit x)) (fst (extractDigit x))

extractDigit :: Integer -> (Integer, Integer)
extractDigit i = let rem = (i `mod` 10) in ((i - rem) `div` 10, rem)