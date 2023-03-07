import System.IO

data Section = Section {getA :: Int, getB :: Int, getC :: Int}
type RoadSystem = [Section] 

data Label = A | B | C deriving Show
type Path = [(Label, Int)]

roadPath :: (Path, Path) -> Section -> (Path, Path)
roadPath (pathA, pathB) (Section a b c) = 
    let pathToA = sum $ map snd pathA
        pathToB = sum $ map snd pathB
        pathToNextA = pathToA + a
        crossPathToA = pathToB + b + c
        pathToNextB = pathToB + b
        crossPathToB = pathToA + a + c
        nextPathToA = if pathToNextA <= crossPathToA 
                        then (A,a):pathA
                        else (C,c):(B,b):pathB
        nextPathToB = if pathToNextB <= crossPathToB 
                        then (B,b):pathB
                        else (C,c):(A,a):pathA
    in (nextPathToA, nextPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (bestPathA, bestPathB) = foldl roadPath ([],[]) roadSystem
    in if sum (map snd bestPathA) <= sum (map snd bestPathB)
            then reverse bestPathA
            else reverse bestPathB

groupByN :: Int -> [a] -> [[a]]
groupByN 0 _ = undefined
groupByN _ [] = []
groupByN n xs = take n xs : groupByN n (drop n xs)

main = do
    contents <- getContents
    let threes = groupByN 3 (map read $ lines contents)
        roadSystem = map (\[a,b,c] -> Section a b c) threes
        bestPath = optimalPath roadSystem
        pathString = concatMap (show . fst) bestPath
        pathCost = sum $ map snd bestPath
    putStrLn $ "The best path is: " ++ pathString
    putStrLn $ "The cost is: " ++ show pathCost

