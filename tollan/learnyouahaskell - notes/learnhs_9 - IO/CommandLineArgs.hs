{- import System.Environment (getArgs, getProgName)

main = do
    args <- getArgs -- getArgs :: IO [String] ... is an I/O action that will get the arguments the program was run with.
    -- It will have as its contained result a list with the arguments... [arg1, arg2, ...] :: [String] -> IO ()
    progName <- getProgName -- getProgName :: IO String ... is an I/O action that contains the program name.
    putStr "The program name is: "
    putStr progName
    putStrLn "\nThe arguments are:"
    mapM putStrLn args -}
-- Run the above code like this ... runHaskell .\CommandLineArgs.hs arg1 arg2 "multi word arg"
--The program name is: CommandLineArgs.hs     The arguments are: arg1 arg2 multi word arg

-- ***** Lets use Command line args to make a program that adds, views or removes todo items from a specified file
import System.IO
import System.Directory (renameFile, removeFile)
import System.Environment (getArgs)
import Data.List (lookup, delete)

dispatch :: [(String, [String] -> IO())] -- Make a dispatch association that maps from commands to functions
dispatch = [ ("add", add),
            ("view", view),
            ("remove", remove),
            ("bump", bump) ]
-- add, view, remove and bump are functions that take arguments (Strings) and return some IO action
main = do
    (command:args) <- getArgs -- Extract the first argument (the command) by pattern matching.
    -- E.g., CmdLineArgs add todo.txt "Do sth" ... Here 'add' is the command and the rest are 'function arguments'
    action (lookup command dispatch) args -- search for the command/action given by the user and return the function
    -- call the 'action' function with Just the appropiate function or Nothing (lookup :: Maybe a)

action :: Maybe ([String] -> IO()) -> [String] -> IO()
action (Just func) args = func args -- call the action function with the rest of the arguments list provided by the user
action Nothing _ = do putStr "Error, command not found!" -- if the cmd/function is not found, output this error text


-- Lets implement the functions to be dispatched
-- runHaskell .\CommandLineArgs.hs add todo.txt "Do some stuff"
add :: [String] -> IO() -- takes a String array and returns an IO action that appends to a file
add [fileName, todoItem] = appendFile fileName (todoItem ++ "\n") -- append (todoItem + \n) at the end of the file

-- runHaskell .\CommandLineArgs.hs view todo.txt   
view :: [String] -> IO() -- returns an IO action that displays the contents of a file
view [fileName] = do 
                    content <- readFile fileName
                    let numberedTasks = zipWith (\no task -> show no ++", "++ task) [0..] (lines content)
                    putStr $ unlines numberedTasks

-- runHaskell .\CommandLineArgs.hs remove todo.txt 3
remove :: [String] -> IO()
remove [fileName, rmIndex] = do 
                             -- if not provided an index
                            {- putStrLn "Which task do you want to remove?"
                            rmIndex <- getLine -}
                            content <- readFile fileName
                            (tempName, tempHandle) <- openTempFile "." "temp"
                            let todoItems = lines content
                                newTodoItems = delete (todoItems !! read rmIndex) todoItems
                            hPutStr tempHandle $ unlines newTodoItems
                            hClose tempHandle
                            removeFile fileName
                            renameFile tempName fileName

bump :: [String] -> IO()
bump [fileName, bpIndex] = do 
                            content <- readFile fileName
                            (tempName, tempHandle) <- openTempFile "." "temp"
                            let todoItems = lines content
                                bumpedItem = todoItems !! read bpIndex
                                newTodoItems = delete bumpedItem todoItems
                            hPutStr tempHandle $ unlines $ bumpedItem:newTodoItems
                            hClose tempHandle
                            removeFile fileName
                            renameFile tempName fileName
                             
                        
