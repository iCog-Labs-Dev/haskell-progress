-- ** Input and Output
-- We compile our program using this command: ghc --make IO_Basics
-- Then we run the exe file created in the same folder: ./IO_Basics

-- Or we can easily run this using: runhaskell IO_Basics.hs

{- main = putStrLn "Hello World" -} -- This outputs 'Hello World' after being executed

-- :t putStrLn ... putStrLn :: String -> IO () .... 'putStrLn' reads a string and returns an IO action
-- that has a result type of '()' i.e., empty tuple. An IO action usually contains a retval instead of '()'

-- ** We can use 'do' syntax to glue several IO actions into one
{- main = do -- The type of the action is IO because of the last IO action inside
    putStrLn "Hello, what's your name?"
    name <- getLine -- :t getLine ... getLine :: IO String ... is an IO action that returns a type String, but :t name :: String
    putStrLn ("Hello " ++ name ++ ", if that's your real name?") -}
        
-- IO actions are impure because they are not guranteed to have the same result every time
-- nameTag = "Hello, my name is " ++ getLine  .. This is not valid syntax because '++' takes two lists of the same type
-- here its (String ++ IO String)

-- Every IO action has a result encapsulated with it, so we can aslo di this
{- main = do
    something <- putStrLn "Hello, what's your name" -- something will have a value of ()
    name <- getLine
    putStrLn ("Hello " ++ name) -} --  The last statement in a 'do' block must be an expression (can't be bound to a name)

-- ** The 'let' keyword
{- import Data.Char (toUpper)

main = do
    putStrLn "What are your first and last names?"
    fname <- getLine
    lname <- getLine
    let fnameUpper = map toUpper fname
        lnameUpper = map toUpper lname
    putStrLn ("Hello, " ++ fnameUpper ++" "++ lnameUpper) -}
-- Notice that '<-' is used for binding rewults of IO actions, while 'let' is used binding pure expressions
-- (let fname = getLine would have just called the getLine IO action a different name)

-- ** Recursion on main
{- main = do
    putStrLn "Enter Some Text..."
    text <- getLine
    if null text
        then return () -- this 'return' statement makes an IO action out of a pure value, but the IO action does nothing 
        else do putStrLn $ reverseWords text -- Else, execute this 'do' block (to do multiple IO actions at once)
                main -- perform main recursively ('main' is also an IO action)
reverseWords :: String -> String
reverseWords = unwords . map reverse . words -}

-- ** 'return' doesn't terminate execution of a do block. They just encapsulate a result that doesn't do anything unless
-- bound to a name by combining it with '<-'
{- main = do
    putStrLn "Say something ..."
    return ()
    return "This is not bound to a name"
    name1 <- return "This is bound to a name" -- it's easier to do: let name1 = "This is ..."
    sth <- getLine
    return 123
    putStr (name1 ++ ". Also, return doesn't terminate execution. You said: " ++ sth) -}

-- ** IO program that takes an input string and outputs its capitalized version
-- ** If you do cat file.txt | ./capslocker, it will capitalize the contents of file.txt
-- ** The '|' character allows us to pipe the output of one program to the input of another
{- import Data.Char (toUpper)
import Control.Monad (forever)

main = do
    forever $ do
        -- putStrLn "\n Enter text: " -- we don't want to include this text on every run of the code (every other line)
        txt <- getLine
        putStrLn $ map toUpper txt -}

-- ** Some basic IO functions
import Data.Char (isAlphaNum)
import Control.Monad (when, forever, forM)
main = do 
    putStr "Hello, "  -- putStr doesn't jump to a new line
    putChar 'h'
    putChar 'i'
    putChar '\n'

    print True -- runs 'show' on a value and outputs it, just like: (putStrLn . show)
    print [1,2,3] -- gchi uses print to display values on the terminal E.g., (3 ... print 3)
    print "Why print the double quotes?" -- we usually just use putStrLn to print strings without the double quotes
    
    putStrLn "Enter a non-alphanumeric character..."
    chr <- getChar -- 
    if isAlphaNum chr 
        then return ()
        else do putStr "You entered: "
                putChar chr
                return ()
    -- We can implement the above 'if' simply using 'when'
    putStrLn "\nEnter another one"
    chr2 <- getChar
    when (not $ isAlphaNum chr2) $ do-- when has to be imported from Control.Monad
            putChar chr2 -- if not, it will return ()
    
    -- sequence takes a list of IO actions, performs them sequentially and returns their results in a list
    putStrLn "Enter two lines two times."
    a1 <- getLine
    a2 <- getLine
    print [a1, a2]
    -- is the same as
    seq1 <- sequence [getLine, getLine]
    print seq1
    -- We usually use sequence when we map over lists using functions like 'print' & 'putStr'.
    -- doing map print [1,2,3] will create a list of IO actions: [print 1, print 2, print 3]
    putStrLn "This prints a bunch of 1,2,3's without warning."
    sequence (map print [1,2,3]) -- transforms the list of IO actions into one IO action
    mapM print [1,2,3] -- this does the same thing
    -- In GCHI,the above two lines will print out the result for print: [(),(),()] ..:t print :: Show a => a -> IO ()
    mapM_ print [1,2,3] -- this also does the same thing but throws away the result later.

    -- 'forever' takes an IO action and returns an IO action ... forever!!! 
    {- forever $ do
        putStrLn "Enter something ..."
        sth2 <- getLine
        putStrLn ("You entered: " ++ sth2) -}
    
    -- 'forM' is just like mapM, but with its parameters switched around. We usally use forM when we map a sequence of
    -- actions we define on the spot using the 'do' notation. Here's an example:
    numbers <- forM [1,2,3] (\n -> do -- the lambda function takes a number and returns an IO action
        putStrLn ("How is "++ show n ++" pronounced?")
        pr <- getLine -- This unpacks the result to 'pr'
        return pr -- This repacks 'pr' as an IO result
        -- Whatever is returned here will be the result of the IO action inside the 'do' block
        -- We could have only written 'getLine' and it would work 
        )
    putStr "You pronounce 1,2,3 as: "
    mapM putStr numbers    -- numbers is just a normal list that holds strings.
    -- or forM numbers putStr ... putStr is an IO action that takes a String and displays it on the terminal
    --To reiterate, I/O actions are values much like any other value in Haskell. We can pass them as parameters to
    -- functions and functions can return I/O actions as results. They are performed when fall into the main function
    -- (or are the result in a GHCI line).
