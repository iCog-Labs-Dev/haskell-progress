{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.List (elemIndex)
import Data.Maybe
import Data.Text qualified as T
import Input (input)

type Coord = (Int, Int)

type Step = Int

type Direction = Char

type Move = (Direction, Step)

bearings :: String
bearings = "NESW"

split :: String -> [String]
split input = map T.unpack $ T.splitOn ", " (T.pack input)

parseStep :: String -> Move
parseStep (x : xs) = (x, read xs :: Int)

toMoves :: Direction -> [String] -> [Move]
toMoves direction [] = []
toMoves direction (x : xs) = (newDirection, snd step) : next
  where
    step = parseStep x
    newDirection = case fst step of
      'R' -> bearings !! ((fromJust (elemIndex direction bearings) + 1) `mod` 4)
      'L' -> bearings !! ((fromJust (elemIndex direction bearings) - 1) `mod` 4)     
    next = toMoves newDirection xs

move :: Coord -> [Move] -> Coord
move coord [] = coord
move (x, y) ((d, s) : xs)
  | d == 'N' = move (x, y - s) xs
  | d == 'S' = move (x, y + s) xs
  | d == 'E' = move (x + s, y) xs
  | d == 'W' = move (x - s, y) xs

distance :: Coord -> Int
distance (x, y) = abs x + abs y
  
  
main :: IO ()
main = do
    print $ toMoves 'N' $ split input
    print $ move (0, 0) (toMoves 'N' $ split input)
    print $ distance $ move (0, 0) (toMoves 'N' $ split input)