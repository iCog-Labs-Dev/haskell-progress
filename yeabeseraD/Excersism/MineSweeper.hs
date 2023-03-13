module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = let emptyCells = [(r,c) | r <-[0..length board -1], 
                                           c <- [0..length (head board) - 1], 
                                           board !! r !! c == ' ']
                     tempBoard = foldr replace board emptyCells
                in (map . map) (\x -> if x == '0' then ' ' else x) tempBoard

type Position = (Int,Int)
type Board = [String]

countMines :: Position -> Board -> Int
countMines (row,col) board = let cellIndex= [(r,c) | r <- [row-1..row+1], r >= 0, r < length board,
                                                     c <- [col-1..col+1], c >= 0 , c < length (head board)]
                                 adjacentCells = [board !! r !! c | (r,c) <- cellIndex, r /= row || c /= col]
                             in length $ filter (=='*') adjacentCells

replace :: Position -> Board -> Board
replace (row, col) board = let count = countMines (row,col) board
                               currentRow = (board !! row)
                               updatedRow = take col currentRow ++ show count ++ drop (col + 1) currentRow
                            in take row board ++ [updatedRow] ++ drop (row + 1) board
board = [" * * ",
        "  *  ",
        "  *  ",
        "     "]