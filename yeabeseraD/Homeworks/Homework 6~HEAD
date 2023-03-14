listOffibs = 0:1:zipWith (+) listOffibs (tail listOffibs)
fibs n = listOffibs !! (n-1)

fibs' 0 = 0
fibs' 1 = 1
fibs' n = fibs'(n-1) + fibs'(n-2)
