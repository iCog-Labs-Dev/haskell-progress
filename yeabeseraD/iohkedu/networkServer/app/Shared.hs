module Main where

import Data.IORef
import Control.Monad
import Control.Concurrent
import Control.Concurrent.STM
import System.Random

main :: IO ()
main = do
    accounts <- mapM newTVarIO [1000, 2500]
    total <- getTotal accounts
    _ <- forkIO $  monitor accounts total
    replicateM_ 100000 (randomTransfer accounts)
    threadDelay 5000000

thread :: IORef Int -> Int -> IO ()
thread ref n = forever $ do 
    writeIORef ref n
    m <- readIORef ref
    when (m/=n) $ print (m,n)

type Account = TVar Integer

getTotal :: [Account] -> IO Integer
getTotal accounts =atomically $ sum <$> mapM readTVar accounts

monitor :: [Account] -> Integer -> IO ()
monitor accounts expected = do
    actual <- getTotal accounts
    if actual /= expected
        then do 
            putStrLn $ "expected " ++ show expected ++ " actual "++ show actual
            monitor accounts actual
        else monitor accounts expected

transfer :: Account -> Account -> Integer -> STM (IO ())
transfer from to amount = do
    fOld <- readTVar from
    let fNew = fOld - amount
    if fNew < 0 
        then return $ putStrLn "Insufficient amount"
        else do
            writeTVar from fNew
            modifyTVar to (+    amount)
            return $ putStrLn $ "Transferred " ++ show amount

randomTransfer :: [Account] -> IO ()
randomTransfer accounts = do
    let n = length accounts
    from <- randomRIO (0, n-1)
    to <- randomRIO (0, n-1)
    amount <- randomRIO (1,1000)
    join $ atomically $ transfer (accounts !! from) (accounts !! to) amount



        

