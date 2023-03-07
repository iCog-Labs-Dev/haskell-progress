-- C. Haile !!!
-- time limit per test 1 second
-- memory limit per test 256 megabytes
-- input standard input
-- output standard output
-- Nati B wants to get the information of all the group 42 students from Hailemariam - a head of education for Group 42. Haile stores all his students' information in a grid. Each student's information is stored in a new row and each column represents specific information about the student in a particular row. He has three columns for - Full names, emails, and phone numbers respectively. But to save memory space he separates these columns with Hashes(#). He did not want to do the work of separating them so he just sent the information as it is to Nati B.

-- Nati B wants you to help him organize the data in a manageable fashion.

-- Input
-- The first line contains an integer N - the number of students in group 42 ( 1 <= N <= 28).

-- The following N lines contain a string s. S can only contain alphabets, digits, and @, # symbols. the length of each s can be at most 100.

-- Output
-- Print N lines that contain the students' First Name, Phone Number, and Email separated by Spaces.

-- Example

-- input
-- 2
-- AdmasTerefe#0987654363#admas@gmail.com
-- AmanuelYihune#091234567#aman@gmail.com

-- output
-- AdmasTerefe 0987654363 admas@gmail.com
-- AmanuelYihune 091234567 aman@gmail.com



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
