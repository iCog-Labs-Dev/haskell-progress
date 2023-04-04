 
 -- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197214897

main :: IO ()
main = do 
    i <- fmap (\x -> read x :: Integer) getLine
    let j = floor $ sqrt $ fromIntegral i
    print $ minimum [ 2 * (x + i `div` x) | x <- [j,j-1..1], i `mod` x == 0] 
    
