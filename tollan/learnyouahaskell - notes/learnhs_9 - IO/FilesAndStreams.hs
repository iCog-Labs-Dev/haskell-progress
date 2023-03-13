-- ***** 'getContents' is an I/O action that reads everything from standard input until it finds an end-of-file character.
{- import Data.Char (toUpper)
main = do -- do $ forever ... would be an alt to getContents
    content <- getContents -- this code won't return until an eof character is reached (Ctrl + Z) ... like the 'forever' function
    putStr $ map toUpper content -} -- 'getContents' is lazy so it will only read when its required to (when 'putStr' happens)

-- ** 'getLine' reads a line of input, 'getChar' reads only a character
{- main = do
    line <- getLine -- this will terminate after one line of input
    putStrLn $ shortOnly line -}
    
-- This pattern of getting some string from the input, transforming it with a function and then outputting that is so
-- common that there exists a function which makes that even easier, called 'interact'.

-- ***** 'interact' can be used to make programs that are piped some contents into them and then dump some result out.  
-- ( ghc --make .\FilesAndStreams.hs  ... cat file.txt | .\FilesAndStreams.exe)
-- It can also be used to make programs that take a line of input, give back some result then take another line and so on.
-- ( runHaskell FilesAndStreams.hs )

-- ** Short lines only - interact
{- main = interact shortOnly

shortOnly :: String -> String
shortOnly = unlines . filter (\l -> length l < 10) . lines -}

-- This is also possible
-- main = interact $ unlines . filter ((<10) . length) . lines 

-- ** Palindrome - interact
{- main = interact checkPalindrome

checkPalindrome :: String -> String
checkPalindrome = unlines . map (\line -> 
        if line == reverse line 
            then "Palindrome"
            else "Not a palindrome") . lines -}
-- Even though we made a program that transforms one big string of input into another, it acts like we made a program that
-- does it line by line. That's because Haskell is lazy and it wants to print the first line of output as soon as it can.

-- ***** Reading from files
-- Here's how we read the contents of 'file.txt' and display its contents on the terminal
{- import System.IO

main = do
    handle <- openFile "file.txt" ReadMode -- openFile :: FilePath -> IOMode -> IO Handle
    -- ... FilePath = String, IOMode = ReadMode|WriteMode|AppendMode|ReadWriteMode, IO Handle = IO action that returns Handle 
    content <- hGetContents handle -- 'hGetContents' takes a handle and returns IO String
    -- (getContents automatically reads from standard input (terminal), but hGetContents takes a file you read from)
    putStr content
    hClose handle  -}-- tekes a handle and returns an IO action that closes the file

-- Another way of doing this is to use the 'withFile' function (withFile :: FilePath -> IOMode -> (Handle -> IO a) -> IO a)
-- withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a  
-- withFile' path mode f = do  
--     handle <- openFile path mode   
--     result <- f handle  
--     hClose handle  
--     return result -- so that our I/O action encapsulates the same result as the one we got from f handle
{- import System.IO
main = do
    withFile "file.txt" ReadMode (\handle -> do
                                    contents <- hGetContents handle
                                    putStr contents) -}
-- The final IO action 'withFile' returns will open that file, passes the handle to our function and then close the file.
-- It returns the same result as the encapsulated lambda function that takes a handle and returns an I/O action. 

-- ** Just like we have hGetContents that works like getContents but for a specific file, there's also hGetLine,
-- hPutStr, hPutStrLn, hGetChar

-- ** Loading files and then treating their contents as strings is so common that we have these three functions to make
-- our work even easier: readFile, writeFile, appendFile. The files are also automatically closed.
{- import System.IO
import Data.Char (toUpper, toLower)
main = do
    content <- readFile "file.txt" -- readFile :: FilePath -> IO String
    writeFile "file1.txt" (map toUpper content) -- writeFile :: FilePath -> String -> IO ()
    appendFile "file1.txt" ("\n" ++ map toLower content)  -}-- appendFile :: FilePath -> String -> IO ()

-- ***** Buffering
-- For text files, the default buffering is line-buffering. That means the smallest part of the file to be read at once
-- is one line. We can adjust the buffering using 'hSetBuffering' function. hSetBuffering :: Handle -> BufferMode -> IO ()
-- BufferMode = NoBuffering (char by char, accesses disk every time) | LineBuffering (default) | BlockBuffering (Maybe Int)
-- (Just bytes, Nothing - OS decides).
{- import System.IO
main = do
    withFile "file.txt" ReadMode (\handle -> do
                                    hSetBuffering handle $ BlockBuffering (Just 2048)
                                    content <- hGetContents handle
                                    putStr content) -}

-- ** 'hFlush' - takes a handle and returns an I/O action that will flush the buffer of the file associated with the handle

import System.IO
import System.Directory (removeFile, renameFile)
import Data.List (delete)

main = do
    handle <- openFile "todo.txt" ReadMode
    (tempName, tempHandle) <- openTempFile "." "temp" -- temp file will be created on current dir and named 'temp' plus some random chars
    -- we create a temp file instead of another txt file not to overwrite anything
    contents <- hGetContents handle
    let todoTasks = lines contents
        numberedTasks = zipWith (\no task -> show no ++" - "++ task) [0..] todoTasks -- add numbers to each task (0 - task1, 1 - task2,...)
    putStr $ unlines numberedTasks -- or ... mapM putStrLn numberedTasks
    putStrLn "Which task do you want to delete?"
    numberString <- getLine -- Take input from user
    let newTodoItems = delete (todoTasks !! (read numberString)) todoTasks -- delete the task
    hPutStr tempHandle $ unlines newTodoItems
    hClose handle
    hClose tempHandle
    removeFile "todo.txt"
    renameFile tempName "todo.txt"


