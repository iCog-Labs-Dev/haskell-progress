floorSqrt :: Int -> Int
floorSqrt = floor . sqrt . fromIntegral

factors :: Int -> Int
factors 1 = 1
factors n = length [x | x <- [1..n],mod n x == 0]

factorSum :: [Int] -> Int
factorSum = sum . map factors

toint :: String -> Int
toint x = read x ::Int

main :: IO ()
main = do 
  n <- fmap toint getLine
  print $ factorSum [1..n]