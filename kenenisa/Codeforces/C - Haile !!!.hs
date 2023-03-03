import qualified Data.Text as T
 
testCases :: Int -> IO ()
testCases 0 = return () 
testCases n = do
        a <- getLine
        let theText = T.pack a
        let tag = T.pack "#"
        putStrLn $ T.unpack $ T.unwords $ T.splitOn tag theText
        testCases (n-1)
 
main :: IO ()
main = do 
    t <- getLine
    testCases (read t :: Int)
