
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197217074

lucky :: String -> Bool
lucky = all (`elem` "47") . show . foldr (\x acc -> if x `elem` "47" then acc + 1 else acc) 0

main :: IO ()
main = do
    n <- getLine
    putStrLn $ if lucky n then "YES" else "NO"