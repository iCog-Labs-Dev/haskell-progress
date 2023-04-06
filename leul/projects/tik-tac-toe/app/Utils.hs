module Utils where

import System.Random

rand :: Int -> Int -> IO Int
rand start end = getStdRandom $ randomR (start, end)

drawBoard :: [String] -> String
drawBoard xs =
  head xs
    ++ " | "
    ++ xs !! 1
    ++ " | "
    ++ xs !! 2
    ++ "\n"
    ++ "---------\n"
    ++ xs !! 3
    ++ " | "
    ++ xs !! 4
    ++ " | "
    ++ xs !! 5
    ++ "\n"
    ++ "---------\n"
    ++ xs !! 6
    ++ " | "
    ++ xs !! 7
    ++ " | "
    ++ xs !! 8
    ++ "\n"
