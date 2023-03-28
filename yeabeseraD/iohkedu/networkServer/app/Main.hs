module Main where

import Network
import Control.Concurrent
import Data.Char
import Control.Monad
import System.IO

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
    forever $ do
        l <- hGetLine h
        hPutStrLn h (map toUpper l)

