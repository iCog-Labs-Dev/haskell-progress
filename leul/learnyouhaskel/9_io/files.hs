import Control.Exception (handle)
import Control.Monad (forever)
import System.IO
import Data.Char

main = do
  fileHandle <- openFile "somefile.txt" ReadMode
  content <- hGetContents fileHandle
  putStrLn content
  hClose fileHandle

  withFile
    "somefile.txt"
    ReadMode
    ( \handle -> do
        content <- hGetContents handle        
        putStrLn content
    )

  content <- readFile "somefile.txt"
  writeFile "newfile.txt" (map toUpper content)
