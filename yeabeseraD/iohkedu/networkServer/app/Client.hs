module Main where

import Network
import Control.Monad
import System.IO

main :: IO ()
main = do
    h <- connectTo "127.0.0.1" (PortNumber 8657)
    hSetBuffering h LineBuffering
    forever $ do
        l <- getLine
        hPutStrLn h l
        response <- hGetLine h
        putStrLn $ "Response: " ++ response