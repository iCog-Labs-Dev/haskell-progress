import System.Directory (getDirectoryContents, doesDirectoryExist)
import Control.Monad (forM)
import System.FilePath ((</>))
import System.Directory.Internal.Prelude (getArgs)
import qualified Data.Text as T

main :: IO ()
main = do args <- getArgs
          case args of
            [] -> putStrLn "Usage: find (file name | folder name)"
            _  -> do paths <- getContentsRecursive "."
                     mapM_ (putStrLn . T.unpack) (filter (T.isInfixOf (T.pack (head args))) (map T.pack paths))
                     
          

showAll :: IO ()
showAll = do paths <- getContentsRecursive "."
             mapM_ putStrLn paths


getContentsRecursive :: FilePath -> IO [FilePath]
getContentsRecursive root = do     
    dirs <- getDirectoryContents root

    paths <- forM (filter (`notElem` [".", ".."]) dirs) (\dirname -> do
        let path = root </> dirname
        isDir <- doesDirectoryExist path
        if isDir
            then getContentsRecursive path
            else return [path]

        )
    
    return $ concat paths