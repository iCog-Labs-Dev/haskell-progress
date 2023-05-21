n = a `div` length xs
   where
   a = 10
   xs = [1,2,3,4,5]

last' xs = drop (length xs - 1) xs

init' xs = take (length xs -1) xs