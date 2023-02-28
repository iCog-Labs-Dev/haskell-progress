
main = interact respond

respond :: String -> String
respond content = unlines $ map (\x -> if isPalindrome x 
                                       then "Yes it's a palindrome.\n"
                                       else "Sorry, it's not.\n")  $ lines content
    
    

isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == reverse xs
