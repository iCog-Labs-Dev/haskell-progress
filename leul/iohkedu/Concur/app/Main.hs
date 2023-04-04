module Main (main) where

import Control.Concurrent
import Control.Monad (forever)
import Network.Socket
import System.IO


seconds :: Int
seconds = 1000000

main :: IO ()
main = do
  sock <- socket AF_INET Stream 0
  setSocketOption sock ReuseAddr 1
  bind sock (SockAddrInet 5000 0)
  listen sock 10
  mainloop sock


mainloop :: Socket -> IO ()
mainloop sock = do 
                connection <- accept sock
                forkIO $ runServer connection                
                mainloop sock
              

runServer :: (Socket, SockAddr) -> IO ()
runServer (sock, addr) = do 
      print addr
      hdl <- socketToHandle sock ReadWriteMode
      hSetBuffering hdl NoBuffering
      line <- hGetContents hdl
      print line
      hPutStrLn hdl "HTTP/1.1 200 OK\nContent-Length: 34\nContent-Type: text/html\n\n<html><body>Hello!</body></html>"
      hClose hdl
