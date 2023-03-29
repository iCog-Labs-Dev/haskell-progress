module Main where

import Network
import Control.Concurrent
import Control.Concurrent.STM
import Control.Monad
import System.IO

main :: IO ()
main = do
    s <- listenOn $ PortNumber 8657
    conns <- newTVarIO []
    void $ forkIO $ monitor 0 conns
    putStrLn "Listening on port number 8657..."
    forever $ do
        (h, n, p) <- accept s
        putStrLn $ "accepted hostname: " ++ n ++ " on portnumber: "++ show p
        forkFinally (handleClient h conns) $
            const $ do
                atomically $ modifyTVar conns (filter (/=h))
                hClose h

handleClient :: Handle -> TVar [Handle] -> IO ()
handleClient h conns = do
    hSetBuffering h LineBuffering
    atomically $ modifyTVar conns (h:)
    forever $ do
            _ <- hGetLine h
            handles <- readTVarIO conns
            let clients = length handles
            hPrint h clients

monitor :: Int -> TVar [Handle] -> IO ()
monitor oldCount conns = do
    newCount <- atomically $ do
        hs <- readTVar conns
        let c = length hs
        if c == oldCount
            then retry
            else return c
    
    hs <- readTVarIO conns
    mapM_ (`hPrint` newCount) hs
    monitor newCount conns

