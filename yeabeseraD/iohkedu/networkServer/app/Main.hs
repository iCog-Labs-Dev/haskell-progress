module Main where

import Network
import Control.Concurrent
import Control.Concurrent.STM
import Control.Concurrent.Async
import Control.Monad
import System.IO

main :: IO ()
main = do
    s <- listenOn $ PortNumber 8657
    conns <- newTVarIO 0
    bch <- newBroadcastTChanIO
    void $ forkIO $ monitor 0 conns bch
    putStrLn "Listening on port number 8657..."
    forever $ do
        (h, n, p) <- accept s
        putStrLn $ "accepted hostname: " ++ n ++ " on portnumber: "++ show p
        forkFinally (handleClient h conns bch) $
            const $ do
                atomically $ modifyTVar conns pred
                hClose h

handleClient :: Handle -> TVar Int -> TChan Int -> IO ()
handleClient h conns bch = do
    hSetBuffering h LineBuffering
    atomically $ modifyTVar conns succ
    ch <- atomically $ dupTChan bch
    forever (void $ hGetLine h) `race_` forever (atomically (readTChan ch) >>= hPrint h)

monitor :: Int -> TVar Int -> TChan Int -> IO ()
monitor oldCount conns bch = do
    newCount <- atomically $ do
        currentCount <- readTVar conns
        if currentCount == oldCount
            then retry
            else do
                writeTChan bch currentCount
                return currentCount
    
    monitor newCount conns bch

