module Minesweeper (annotate) where

annotate :: [String] -> [String]
annotate board = removeBorder $ map (\(x, row) -> countMines x row) withIndices
    where bordered = addBorder board
          withIndices = addIndices bordered
          countMines x = map (\(y, cell) -> if cell == ' ' then (surroundingMines (x,y) bordered) else cell ) 
          

addBorder :: [String] -> [String]
addBorder field = map (\row -> "0"++row++"0") topBottom
    where x = length $ head field
          y = length field
          border = take x $ repeat '0'
          topBottom = [border] ++ field ++ [border]

removeBorder :: [String] -> [String]
removeBorder borderField = map (\row -> tail(init row)) topBottom
    where topBottom =  tail (init borderField)

addIndices :: [String] -> [ (Int, [(Int, Char)]) ]
addIndices board = zip [0..] $ map (\row -> zip [0..] row ) board


surroundingMines :: (Int, Int) -> [String] -> Char
surroundingMines (x,y) borderField = if noMines > 0 then head $ show $ noMines else ' '
    where surrounding = [(a,b) | a <- [x-1, x, x+1], b <- [y-1, y, y+1] ]
          noMines = length $ filter (\(x',y') -> (borderField !! x') !! y' == '*' ) surrounding