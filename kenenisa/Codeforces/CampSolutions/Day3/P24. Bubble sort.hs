-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203235158

toInt :: String -> Int
toInt x = read x :: Int

swapItems :: [Int] -> [Int]
swapItems [] = []
swapItems [x] = [x]
swapItems (x:ys@(y:xs))
  | x > y = y : swapItems (x:xs)
  | otherwise = x : swapItems ys

loop :: Int -> [Int] -> IO ()
loop n numbers= do
  let values = swapItems numbers
  putStrLn $ unwords $ map show values
  if n == 0 then return () else loop (n-1) values
main :: IO ()
main = do
    n <- fmap toInt getLine
    numbers <- fmap (map toInt . words) getLine
    loop (n-2) numbers