import Data.List

solveRpn :: String -> Float
solveRpn = head . foldl calculate [] . words
  where calculate (x:y:ys) "*" = (x * y):ys
        calculate (x:y:ys) "+" = (x + y):ys
        calculate (x:y:ys) "-" = (y - x):ys
        calculate (x:y:ys) "/" = (y / x):ys
        calculate (x:y:ys) "^" = (y ** x):ys
        calculate (x:ys) "log" = log x: ys
        calculate xs numberString = read numberString:xs
