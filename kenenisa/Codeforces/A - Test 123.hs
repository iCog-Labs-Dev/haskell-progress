printer :: Int -> IO ()
printer 0 = return ()
printer n = do
    word <- getLine
    putStrLn word
    printer (n-1)
main :: IO ()
main = do
    testCases <- getLine
    let t = read testCases :: Int in
        printer t
