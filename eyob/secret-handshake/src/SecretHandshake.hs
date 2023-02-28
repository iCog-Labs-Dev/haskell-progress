module SecretHandshake (handshake) where

handshake :: Int -> [String]
handshake n
  | n < 0 = []
  | otherwise = if n >= 16 then reverse earlyHandshake else earlyHandshake
  where
    earlyHandshake = map snd $ validBinaries
    validBinaries = filter (\(x, _) -> x == 1) $ binary
    binary = zip (getBinary n) actions
    actions = ["wink", "double blink", "close your eyes", "jump"]

getBinary :: Int -> [Int]
getBinary 0 = []
getBinary n = n `mod` 2 : getBinary (n `div` 2)

findIdx :: [Int] -> Int -> Int
findIdx xs elmt = fst $ head $ filter (\(_, x) -> x == elmt) (zip [0 ..] xs)
