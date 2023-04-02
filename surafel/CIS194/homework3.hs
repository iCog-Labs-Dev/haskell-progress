{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
module Golf where

import Data.List (find, findIndex, intercalate)
import Data.Maybe (fromMaybe)

skips :: [a] -> [[a]]
skips xs = foldl skipper [] [1 .. length xs]
  where
    skipper acc count =
      acc ++ [map snd (filter fst [(cnt `mod` count == 0, x) | (x, cnt) <- zip xs [1 .. (length xs)]])]

localMaxima :: [Integer] -> [Integer]
localMaxima [_] = []
localMaxima [_, _] = []
localMaxima xs = [take 3 xs !! 1 | isMaxima (take 3 xs)] ++ localMaxima (drop 1 xs)
  where
    isMaxima xs = (xs !! 1) > (xs !! 0) && (xs !! 1) > (xs !! 2)

histogram :: [Int] -> String
histogram xs = intercalate "\n" (reverse (foldl doCount [replicate 10 ' '] xs)) ++ "\n==========\n0123456789\n"
  where
    doCount :: [String] -> Int -> [String]
    doCount hist num =
      let bar = fromMaybe (length hist) (findIndex ((== ' ') . (!! num)) hist)
       in if bar < length hist
            then replace' bar hist (replace' num (hist !! bar) '*')
            else replace' bar (hist ++ [replicate 10 ' ']) (replace' num ((hist ++ [replicate 10 ' ']) !! bar) '*')
    replace' :: Int -> [a] -> a -> [a]
    replace' ind lst val = take ind lst ++ [val] ++ drop (ind + 1) lst
