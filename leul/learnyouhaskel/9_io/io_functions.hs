import Control.Monad (forever, forM)
import Data.Char

main = do 
    print True

    result <- sequence [print 1, print 2, putStrLn "3"]
    
    let action = map print [4, 5, 6]

    result <- sequence action
    print result 

    result <- mapM print [1, 2, 3]

    print result
    print "BYe"

    print "Enter some thing"
    x <- getLine
    print x

    print "Enter some thing"
    x <- getLine
    print x

    -- forever $ do print "Hello"
    putStrLn "Lets map!"
    
    colors <- forM [1, 2, 3, 4] (\x -> do 
        putStrLn $ show x ++ " is for "
        getLine)
    
    putStrLn "You associated 1, 2, 3 and 4 with:"
    mapM_ putStrLn colors

    -- interact is forever
    interact toUpper'

toUpper' :: String -> String
toUpper' = map toUpper