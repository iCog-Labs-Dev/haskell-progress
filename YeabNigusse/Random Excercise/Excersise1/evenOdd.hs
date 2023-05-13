--Receive a number and determine whether it is odd or even.
main = do
    putStrLn "Enter a number"
    number <- getLine
    if even (read number) then putStrLn "Your Number is even" else putStrLn "Your number is Odd"