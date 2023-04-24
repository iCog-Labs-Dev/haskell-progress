--  accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203236096

toInt :: String -> Int
toInt x = read x :: Int

findMinItemIndex :: [Int] -> Int -> Int -> Int -> Int
findMinItemIndex [] index _ _ = index
findMinItemIndex (x:xs) index price count
  | x < price = findMinItemIndex xs count x (count+1)
  | otherwise = findMinItemIndex xs index price (count+1)

main :: IO ()
main = do 
  n <- fmap toInt getLine
  numbers <- fmap (map toInt . words) getLine
  print $ findMinItemIndex numbers 0 (head numbers) 0