-- https://exercism.org/tracks/haskell/exercises/secret-handshake/

module SecretHandshake (handshake) where

actions :: [String]
actions = ["wink", "double blink", "close your eyes", "jump"]


toBinary :: Int -> [Int]
toBinary 0 = []
toBinary n = n `mod` 2 : toBinary (floor (fromIntegral n / 2))


toAction :: Int -> [Int] -> [String]
toAction n [] = []
toAction n (x:xs) = case x of 
                        0 -> toAction (n + 1) xs
                        1 -> (actions !! n) : toAction (n + 1) xs


handshake :: Int -> [String]
handshake n 
    | length actionSeq == 5 && actionSeq !! 4 == 1 = reverse $ toAction 0 $ init actionSeq
    | otherwise = toAction 0 actionSeq
    where actionSeq = take 5 $ toBinary n


