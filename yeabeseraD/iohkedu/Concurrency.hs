import Control.Concurrent
import Control.Monad (forever)

numberForever :: Int -> IO ()
numberForever n = do
    mapM_ putStrLn (repeat (show n))

second :: Int
second = 1000000

thread :: Int -> IO ()
thread n = forever $ do
    print n
    threadDelay (second `div` 10)

main :: IO ()
main = do
    mapM_ (forkIO . numberForever) [1,2]
    threadDelay (second * 5)