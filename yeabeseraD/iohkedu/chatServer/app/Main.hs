module Main where

import          Network
import          Control.Concurrent
import          Control.Concurrent.STM
import          Control.Concurrent.Async
import          Control.Monad
import          Data.Set (Set)
import qualified Data.Set as S
import          System.IO

type Name = String
data Message = 
      Joined Name
    | Disconnected Name
    deriving (Show, Eq, Ord, Read)

main :: IO ()
main = do
    s <- listenOn $ PortNumber 8657
    bch <- newBroadcastTChanIO
    names <- newTVarIO S.empty
    putStrLn "Listening on port number 8657..."
    forever $ do
        (h, host, p) <- accept s
        mname <- newTVarIO Nothing
        putStrLn $ "accepted hostname: " ++ host ++ " on portnumber: "++ show p
        forkFinally (handleClient h names mname bch) $ const $ do
            ms <- readTVarIO mname
            case ms of
                Nothing -> return ()
                Just n -> do
                    atomically $ modifyTVar' names (S.delete n)
                    atomically $ writeTChan bch $ Disconnected n
            hClose h

handleClient :: Handle -> TVar (Set Name) -> TVar (Maybe Name) -> TChan Message -> IO ()
handleClient h names mname bch = do
    hSetBuffering h LineBuffering
    ch <- atomically $ dupTChan bch
    n <- negotiateName h names mname
    atomically $ writeTChan ch $ Joined n
    incoming h `race_` outgoing h ch

incoming :: Handle -> IO ()
incoming h = forever $ do
    void $ hGetLine h

outgoing :: Handle -> TChan Message -> IO ()
outgoing h ch = forever $ do
    msg <- atomically $ readTChan ch
    hPrint h msg

negotiateName :: Handle -> TVar (Set Name)-> TVar (Maybe Name) -> IO Name
negotiateName h names mname = go
    where go = do
            hPutStrLn h "What is your name?"
            n <- hGetLine h

            if null n
                then do
                    hPutStrLn h "Name can't be empty!"
                    go
                else do
                    b <- atomically $ do
                        s <- readTVar names
                        if S.member n s
                            then return False
                            else do
                                writeTVar names $ S.insert n s
                                writeTVar mname $ Just n
                                return True
                    if b then return n
                        else  do
                            hPutStrLn h "Name is already taken!"
                            go