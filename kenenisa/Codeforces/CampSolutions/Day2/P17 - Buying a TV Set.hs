
-- accepted solution https://codeforces.com/group/yg7WhsFsAp/contest/419146/submission/197240577

main :: IO ()
main = do
    [a,b,x,y] <- fmap (map (\x -> read x :: Integer) . words) getLine
    let g = gcd x y
    print $ min (div a ( div x g)) (div b (div y g))