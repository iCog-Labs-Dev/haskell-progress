
module WordCount (wordCount) where
import Data.Char
import qualified Data.Map as Map
allowed = ['a' .. 'z'] ++ ['A' .. 'Z'] ++ ['0' .. '9']

cleanUp :: String -> String
cleanUp [] = []
cleanUp [x]
  | x `elem` allowed = [x]
  | otherwise = [' ']
cleanUp [x, y]
  | x `elem` allowed = x : cleanUp [y]
  | otherwise = ' ' : cleanUp [y]
cleanUp (x : y : z : xs)
  | y == '\'' && x `elem` allowed && z `elem` allowed = x : y : z : cleanUp xs
  | x `elem` allowed = x : cleanUp (y:z:xs)
  | otherwise = ' ' : cleanUp (y:z:xs)

lowering :: String -> String
lowering = map toLower

createMap :: [String] -> Map.Map String Int -> Map.Map String Int
createMap xs hashMap = foldl (\ hashMap x -> Map.insert x (Map.findWithDefault 0 x hashMap + 1) hashMap) hashMap xs

wordCount :: String -> [(String, Int)]
wordCount xs = Map.toList (createMap (words $ lowering $ cleanUp xs) Map.empty)
