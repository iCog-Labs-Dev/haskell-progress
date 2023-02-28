
module Queens (boardString, canAttack) where
import Data.List (intercalate)
placeQueen :: Char -> Int -> String -> String
placeQueen _ _ [] = []
placeQueen queen file (x:xs) | file == 0 = queen : placeQueen queen (file-1) xs
                             | otherwise = x : placeQueen queen (file-1) xs

construct :: Int -> Int -> (Int,Int) -> (Int,Int) -> [String] -> [String]
construct 8 8 _ _ _ = []
construct rank file white@(wr,wf) black@(br,bf) (x:xs) | wr == rank = placeQueen 'W' wf x : construct (rank+1) (file+1) white black xs
                                                       | br == rank = placeQueen 'B' bf x : construct (rank+1) (file+1) white black xs
                                                       | otherwise = x : construct (rank+1) (file+1) white black xs

emptyBoard :: [String]
emptyBoard = replicate 8 (replicate 8 '_')

spaceSquares :: String -> String
spaceSquares [] = []
spaceSquares (x:xs) = x: ' ' : spaceSquares xs

configureBoard :: (Int, Int) -> (Int, Int) -> String
configureBoard white black = intercalate "\n" (map (init . spaceSquares) (construct 0 0 white black emptyBoard)) ++ "\n"

boardString :: Maybe (Int, Int) -> Maybe (Int, Int) -> String
boardString Nothing Nothing = configureBoard (-1,-1) (-1,-1)
boardString Nothing (Just black) = configureBoard (-1,-1) black
boardString (Just white) Nothing = configureBoard white (-1,-1)
boardString (Just white) (Just black) = configureBoard white black


directions :: [(Integer, Integer)]
directions = [(0,1),(1,1),(1,0),(1,-1),(0,-1),(-1,-1),(-1,0),(-1,1)]

north (_,-1) _ = False
north (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = north (wr,wf-1) b

northEast (_,-1) _ = False
northEast (8,_) _ = False
northEast (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = northEast (wr+1,wf-1) b

east (8,_) _ = False
east (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = east (wr+1,wf) b

southEast (8,_) _ = False
southEast (_,8) _ = False
southEast (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = southEast (wr+1,wf+1) b

south (_,8) _ = False
south (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = south (wr,wf+1) b

southWest (-1,_) _ = False
southWest (_,8) _ = False
southWest (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = southWest (wr-1,wf+1) b

west (-1,_) _ = False
west (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = west (wr-1,wf) b

northWest (-1,_) _ = False
northWest (_,-1) _ = False
northWest (wr,wf) b@(br,bf) | wr == br && wf ==bf = True
                      | otherwise = northWest (wr-1,wf-1) b


canAttack :: (Int, Int) -> (Int, Int) -> Bool
canAttack w b = north w b || northEast w b || east w b || southEast w b || south w b || southWest w b || west w b || northWest w b
