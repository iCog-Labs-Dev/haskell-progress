import System.IO
import Control.Monad ( forever )

main = do         
        fileHandle <- openFile "somefile.txt" ReadMode
        content <- hGetContents fileHandle
        putStrLn content