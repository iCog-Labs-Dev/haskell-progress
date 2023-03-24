import System.Environment (getArgs)
import IOFunctions (failure, help, modes)
import Control.Exception (catch)

main ::IO ()
main = do
    args <- getArgs
    let action = if null args 
                    then failure args
                    else let (command:arg) = args; result = lookup command modes;
                              action' = case result of
                                Nothing -> help arg
                                (Just function) -> function arg
                              in action'
    action

