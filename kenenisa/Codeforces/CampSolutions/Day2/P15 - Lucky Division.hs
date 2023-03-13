
 -- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197219449

lucky :: [Int]
lucky = [4,7,44,77,47,74,744,474,447,774,747,477,444,777]

main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ if any (\x -> n `mod` x == 0) lucky then "YES" else "NO"