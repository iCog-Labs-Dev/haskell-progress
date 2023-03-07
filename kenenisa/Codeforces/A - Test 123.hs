-- A. Test 123
-- time limit per test1 second
-- memory limit per test256 megabytes
-- inputstandard input
-- outputstandard output
-- At A2SV, we struggled for a while to conduct the QA sessions because of sound quality problems. Emre has imported a speaker from Türkiye to address this issue and asked Dagim, the social media manager, to set it up.

-- Given a list of test phrases dagim used print them back.

-- Input
-- The first line contains a number n, which is the number of test phrases Dagim used to test the speaker. The next n lines contains the test phrases.

-- Output
-- Output the given test phrases one by one.

-- Example

-- input
-- 2
-- test123
-- hello

-- output
-- test123
-- hello


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
