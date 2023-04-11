main :: IO ()
main = do
    interact $ unlines . map show . solve . map read . words

solve :: [Int] -> [Int]
solve (a:b:c:d:e:f:rest) = [app, orange]
              where app = length $ filter (\x -> a <=x && x <= b) $ map (\x -> x + c) $ take e rest
                    orange = length $ filter (\x -> a <=x && x <= b) $ map (\x -> x + d) $ drop e rest
-- Test by using (cat apple.txt |  runhaskell appleAndOrange.hs) command


