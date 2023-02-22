sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

prefixsum :: (Num a) => [a] -> [a]
prefixsum = scanl (+) 0

postfixsum :: (Num a) => [a] -> [a]
postfixsum = scanr (+) 0

-- composition
negativeSqr :: [Double] -> [Double]
negativeSqr = map(negate . (^2))