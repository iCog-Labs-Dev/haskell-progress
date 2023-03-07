-- D. A2SVians
-- time limit per test 1 second
-- memory limit per test 256 megabytes
-- input standard input
-- output standard output
-- The amount of excellent members intrigues us at A2SV. Tell us the number of excellent members we have based on the list of members.

-- However, the drawback is that you will just know each member's name. A member is deemed to be excellent if it contains an equal number of instances of each character in the name and is not flagged as bad by the heads of education.

-- Input
-- Each test case consists of three lines.

-- The first line of each test case contains a single integer n (0 ≤ n ≤ 10**4) — the number of team members.

-- The second line of each test case contains a string containing n strings separated by a space.

-- The third line of each test case contains a string of length m (0 ≤ n ≤ 10**3) containing the names of members flagged as bad separated by a space.

-- The length of the name of each member can not exceed 100.

-- Output
-- For each test case, output the number of excellent members.

-- Example

-- input
-- 4
-- Nazrawi Haile Brook Fitsum
-- Haile Fitsum

-- output
-- 0



filterRepeated :: String -> Bool
filterRepeated [] = True
filterRepeated (x:xs) | x `elem` xs = False
                      | otherwise = filterRepeated xs
main :: IO ()
main = do
    n <- getLine
    students <- getLine
    banned <- getLine
    print $ length $ filter filterRepeated (filter (`notElem` words banned) (words students))
