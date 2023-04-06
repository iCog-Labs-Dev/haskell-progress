module Main where

import Data.Maybe
import Utils

makeMove :: [String] -> Int -> String -> Maybe [String]
makeMove board move player
  | move < 1 || move > 9 || board !! (move - 1) /= " " = Nothing
  | otherwise = Just [if idx == move - 1 then player else board !! idx | idx <- [0 .. 9]]

getPlayerNextMove :: [String] -> String -> IO [String]
getPlayerNextMove board player = do
  _ <- putStrLn "Choose your next move"
  move <- getLine
  case makeMove board (read move :: Int) player of
    Nothing -> getPlayerNextMove board player
    Just b -> return b

isGameComplete :: [String] -> Bool
isGameComplete board
  | isJust $ getWinner board = True
  | otherwise = " " `notElem` board

getWinner :: [String] -> Maybe String
getWinner board
  | head board == board !! 1 && board !! 1 == board !! 2 && board !! 2 /= " " = Just $ head board
  | board !! 3 == board !! 4 && board !! 4 == board !! 5 && board !! 5 /= " " = Just $ board !! 3
  | board !! 6 == board !! 7 && board !! 7 == board !! 8 && board !! 6 /= " " = Just $ board !! 6
  | head board == board !! 3 && board !! 3 == board !! 6 && board !! 6 /= " " = Just $ board !! 3
  | board !! 2 == board !! 4 && board !! 4 == board !! 7 && board !! 6 /= " " = Just $ board !! 4
  | board !! 3 == board !! 5 && board !! 5 == board !! 8 && board !! 6 /= " " = Just $ board !! 5
  | head board == board !! 4 && board !! 4 == board !! 8 && board !! 8 /= " " = Just $ board !! 4
  | board !! 2 == board !! 4 && board !! 4 == board !! 6 && board !! 6 /= " " = Just $ board !! 4
  | otherwise = Nothing

displayResult :: Maybe String -> IO ()
displayResult Nothing = do putStrLn "Draw"
displayResult (Just winner) = do putStrLn $ winner ++ " won!"

gameLoop :: [String] -> String -> IO ()
gameLoop board turn = do
  putStrLn $ drawBoard board
  new_board <- getPlayerNextMove board turn

  if isGameComplete new_board
    then do
      putStrLn $ drawBoard new_board
      displayResult $ getWinner new_board
    else gameLoop new_board (if turn == "X" then "O" else "X")

main :: IO ()
main = do
  let board = [" ", " ", " ", " ", " ", " ", " ", " ", " "]
  putStrLn "Player 1: X"
  putStrLn "Player 2: O"
  gameLoop board "X"
