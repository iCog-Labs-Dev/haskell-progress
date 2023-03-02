countGreen :: String -> Int -> (String,Int)
countGreen [] _ = ([],0)
countGreen (x : xs) c
  | x == 'g' = (xs,c)
  | otherwise = countGreen xs (c + 1)
 
lookForTarget :: String -> Char -> Int -> Int
lookForTarget [] _ val = val
lookForTarget (x : xs) target val
  | target == 'g' = 0
  | x == target = lookForTarget (fst cg) target $ max val $ snd cg
  | otherwise = lookForTarget xs target val
            where cg = countGreen xs 1
 
testCases :: Int -> IO ()
testCases 0 = return ()
testCases n = do
  nAndTarget <- getLine
  lights <- getLine
  let target = last nAndTarget
      l = read (head $ words nAndTarget) :: Int
      looped = take (2 * l) (cycle lights)
  print $ lookForTarget looped target 0
  testCases (n - 1)
 
main :: IO ()
main = do
  t <- getLine
  testCases (read t :: Int)
