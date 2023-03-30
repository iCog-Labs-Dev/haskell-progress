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
    | Broadcast Name String
    | Private Name Name String
    | Kicked Name Name
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
    n <- negotiateName h names mname
    ch <- atomically $ dupTChan bch
    atomically $ writeTChan ch $ Joined n
    incoming h ch n `race_` outgoing h ch n

incoming :: Handle -> TChan Message -> Name -> IO ()
incoming h ch n = go
    where go = do
            msg <- hGetLine h
            case words msg of
                ["/quit"]         -> return ()
                "/tell" : to : xs -> sendMessage (Private n to $ unwords xs) >> go
                ["/kick", whom]   -> sendMessage (Kicked n whom) >> go
                _                 -> sendMessage (Broadcast n msg) >> go
          sendMessage = atomically . writeTChan ch

outgoing :: Handle -> TChan Message -> Name -> IO ()
outgoing h ch n = go
    where go = do
            msg <- atomically $ readTChan ch
            case msg of
                Joined who -> hPutStrLn h ("***"++ who ++ "\t joined ***") >> go
                Disconnected who -> hPutStrLn h ("***" ++ who ++ "\t disconnected ***") >> go
                Broadcast who what
                    | who /= n -> hPutStrLn h (who ++ ": " ++ " "++ what) >> go
                    | otherwise -> go
                Private from to what
                    | n == to -> hPutStrLn h (">>>"++from++": "++what) >> go
                    | otherwise -> go
                Kicked who whom -> do 
                    hPutStrLn h ("###"++who++" kicked "++whom)
                    unless (whom == n) go

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