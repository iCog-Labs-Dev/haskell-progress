import System.IO
import Data.Char
import Data.List


main :: IO()
main = do
  hSetBuffering stdout NoBuffering
  words <- getWords
  putStrLn "File to search:"
  path <- getLine
  text <- readFile path

  let found = findWords words text
  let notfound = words \\ found
  
  mapM_ (\s -> putStrLn $ "\""++s++"\""++" found") found
  mapM_ (\s -> putStrLn $ "\""++s++"\""++" NOT found") notfound

getWords :: IO [String]
getWords = do
  putStrLn "Enter search words:"
  aux

  where 
  aux = do
    putStr "> "
    word <- getLine

    if word == "" then
      return []
    else do
      xs <- aux
      return $ word : xs
findWords :: [String] -> String -> [String]
findWords xs text = [ w | w <- xs, (lower w) `elem` txtwords]
  where
    lower w = map toLower w
    ftxt = filter (\x -> isSpace x || isLetter x) text
    txtwords = map lower $ words ftxt
