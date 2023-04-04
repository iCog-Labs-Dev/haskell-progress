module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import Network.Socket

seconds :: Int
seconds = 1000000

main :: IO ()
main = do
  mapM_ (forkIO . printForever) [1 .. 10]
  threadDelay (5 * seconds)
  print "Hello"

printForever :: (Show a) => a -> IO b
printForever n = forever $ print n