module Main where

import Risk
import Control.Monad.Random

main:: IO ()
main = do
    result <- evalRandIO $ successProb battlefield
    print result
