module Main where

import Network
import Control.Concurrent
import Data.Char
import Control.Monad
import System.IO
import System.IO.Error

main :: IO ()
main = do
    s <- listenOn $ PortNumber 8657
    forever $ do
        (h, n, p) <- accept s
        putStrLn $ "accepted hostname " ++ show n ++ " on portnumber "++ show p
        forkIO (handleClient h)

handleClient :: Handle -> IO ()
handleClient h = do
    hSetBuffering h LineBuffering
    catchIOError (forever $ do
            l <- hGetLine h
            hPutStrLn h (map toUpper l))
            (\e -> putStrLn $ "Something went wrong: \n\t " ++ show e ++ "\n I'm out of here.")

