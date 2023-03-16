import Data.List
import Control.Monad (liftM, foldM)
solveRpn :: String -> Maybe Double
solveRpn xs = do
  [result] <- foldM calculate [] (words xs)
  return result
  
calculate :: [Double] -> String -> Maybe [Double]
calculate (x:y:ys) "*" = return((x * y):ys)
calculate (x:y:ys) "+" = return((x + y):ys)
calculate (x:y:ys) "-" = return ((y - x):ys)
calculate (x:y:ys) "/" = return ((y / x):ys)
calculate (x:y:ys) "^" = return ((y ** x):ys)
calculate (x:ys) "log" = return (log x: ys)
calculate xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: String -> Maybe Double
readMaybe numberString = case reads numberString of
  [(x, "")] -> return x
  _ -> Nothing
