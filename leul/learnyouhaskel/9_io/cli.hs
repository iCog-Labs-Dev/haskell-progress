import System.Directory.Internal.Prelude (getArgs)
import System.Environment (getProgName)
main = do
    progName <- getProgName
    args <- getArgs
    putStrLn $ "Program name: " ++ progName
    putStrLn $ "Arguments: " ++ show args
