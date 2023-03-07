import Data.Char(toUpper)
import Control.Monad 
--HELLO WORLD

--main = putStrLn "Hello world"

-- here is what we do to run this program
-- ghci --make InputOutput then ./InputOutput

-- to add more input output action into our program we use do
tellFortune :: String -> String
tellFortune name = "hey, "++ name ++ "! Your future will be great "
{--
main = do
    putStrLn  "Hello, what is your name?"
    name <- getLine
    putStrLn  ("Hey "++ name ++ " you rock!")
    putStrLn "hello, what is your name?"
    name <- getLine
    putStrLn $ "Read this carfully, because this is your future: " ++ tellFortune name
--}

{--
main = do
    putStrLn "what is your first name"
    firstName <- getLine
    putStrLn "what is your last name"
    lastName <- getLine
    let bigName = map toUpper firstName
        smallName = map toUpper lastName
    putStrLn $ "hey " ++ bigName  ++" "++ smallName ++ " How are you?"
--}
{--
main :: IO b
main = do
    line <- getLine
    if null line then return () else do putStrLn $ reverseWord line
    main
reverseWord :: String -> String
reverseWord = unwords . map reverse .words
--}
{--
main :: IO ()
main = do  
    return ()  
    return "HAHAHA"  
    line <- getLine  
    return "BLAH BLAH BLAH"  
    return 4  
    putStrLn line  --All these returns do is that they make I/O actions that don't really do anything
--}
{--
main :: IO ()
main = do
    putStr "hey "
    putStr "yeab"
--}
{-
main :: IO ()
main = do 
    putChar 'h'
    putChar 'e'
    putChar 'y'
--}

putStr' :: String -> IO()
putStr' [] = return ()
putStr' (x:xs) = do
    putChar x
    putStr' xs
{--
main :: IO ()
main = do
    c <- getChar
    if c /= ' '
        then do 
            putChar c
            main
        else
            return () --hello sir
                      --hello
--}
{--
main :: IO ()
main = do
    putStrLn "enter number"
    number <- getLine
    print (read number :: Int)
--}
{--
main :: IO ()
main = do 
    c <- getChar
    when (c /= ' ')$ do
        putChar c
        main
--}
main :: IO ()
main = do
    myList <- sequence [getLine, getLine, getLine]
    print myList