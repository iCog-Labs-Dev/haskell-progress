-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/355494/submission/203238419

findMinItemIndex :: [Int] -> Int -> Int -> Int -> Int
findMinItemIndex [] index _ _ = index
findMinItemIndex (x:xs) index price count
  | x < price = findMinItemIndex xs count x (count+1)
  | otherwise = findMinItemIndex xs index price (count+1)

toInt :: String -> Int
toInt x = read x :: Int

loop :: Int -> Int -> [Int] -> IO ()
loop i n numbers = do
  if i > 0 then putStrLn $ unwords $ map show numbers else return ()
  if i < n then
    let j = findMinItemIndex (drop i numbers) i (numbers !! i) i
        swapped = if i /= j  then swapElementsAt i j numbers else numbers
    in
      loop (i+1) n swapped
  else
    return ()

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt i j xs = left ++ [elemJ] ++ middle ++ [elemI] ++ right
  where 
    elemI = xs !! i
    elemJ = xs !! j
    left = take i xs
    middle = take (j - i - 1) (drop (i + 1) xs)
    right = drop (j + 1) xs

main :: IO ()
main = do 
  n <- fmap toInt getLine
  numbers <- fmap (map toInt . words) getLine
  loop 0 (n-1) numbers