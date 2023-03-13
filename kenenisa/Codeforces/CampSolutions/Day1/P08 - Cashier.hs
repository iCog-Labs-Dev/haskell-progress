
-- accepteed at https://codeforces.com/group/yg7WhsFsAp/contest/355490/submission/196657915

getIntList :: String -> [Int]
getIntList = map read . words

findSpace :: [(Int,Int)] -> Int -> Int -> Int -> Int
findSpace [] start a l = (l - start) `div` a 
findSpace (x:xs) start a  l = ((fst x - start) `div` a) +  findSpace xs (snd x) a l 

nLine :: Int -> [(Int,Int)] -> Int -> Int -> IO ()
nLine 0 xs a l = do
    print $ findSpace (reverse xs) 0 a l
nLine n xs a l = do
  line <- getLine
  let [x,y] = getIntList line
  nLine (n-1) ((x,x+y):xs) a l
main :: IO ()
main = do
  first <- getLine
  let [n,l,a] = getIntList first
  nLine n [] a l
