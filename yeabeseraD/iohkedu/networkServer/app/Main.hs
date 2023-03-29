module Main where

import Network
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO

main :: IO ()
main = do
    s <- listenOn $ PortNumber 8657
    conns <- newTVarIO 0
    putStrLn "Listening on port number 8657..."
    forever $ do
        (h, n, p) <- accept s
        putStrLn $ "accepted hostname: " ++ n ++ " on portnumber: "++ show p
        forkFinally (handleClient h conns) $
            const $ do
                atomically $ modifyTVar conns pred
                hClose h

handleClient :: Handle -> TVar Int -> IO ()
handleClient h conns = do
    hSetBuffering h LineBuffering
    atomically $ modifyTVar conns succ
    forever $ do
            _ <- hGetLine h
            clients <- readTVarIO conns
            hPrint h clients

