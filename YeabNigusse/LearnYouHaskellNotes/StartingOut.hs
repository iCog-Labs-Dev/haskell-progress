-- in haskell if statements are expression
-- they always return some value

doubleMe :: Int -> Int
doubleMe x = if x > 100 then x else x*2

