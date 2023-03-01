module Minesweeper (annotate) where

directions :: [(Int, Int)]
directions  = [(1,0),(0,1),(-1,0),(0,-1),(1,1),(-1,1),(1,-1),(-1,-1)]

counts :: [String] -> Int -> Int -> Int -> Int ->[(Int,Int)]-> Int
counts _ _ _ _ _ [] = 0
counts board r c boundR boundC (x:xs) = found + counts board r c boundR boundC xs  
                                where 
                                    moveR = r + fst x
                                    moveC = c + snd x
                                    found | moveR > -1 && moveR < boundR && moveC > -1 && moveC < boundC && ((board !! moveR) !! moveC) == '*' = 1
                                          | otherwise = 0
                                           


cols :: String -> [String] -> Int -> Int -> String
cols [] _ _ _ = []
cols (x:xs) board r c = replaceCounted : cols xs board r (c+1)
                    where counted | ((board !! r) !! c) == '*' = 0
                                  | otherwise = counts board r c (length board) (length $ head board) directions
                          replaceCounted | counted > 0 = head $ show counted
                                         | otherwise = x

rows :: [String] -> [String] -> Int -> [String]
rows [] _ _ = []
rows (x:xs) board r = cols x board r 0 : rows xs board (r+1)

annotate :: [String] -> [String]
annotate [] = []
annotate [""] = [""]
annotate board = rows board board 0
