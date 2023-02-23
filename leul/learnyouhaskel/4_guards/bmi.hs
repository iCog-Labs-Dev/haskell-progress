bmi :: (RealFloat a) => a -> [Char]
bmi x
    | x < 18.5 = "Under weight"
    | x < 25.0 = "Normal"
    | x < 30.0 = "OK"
    | otherwise = "Concerning"


bmi' :: (RealFloat a) => a -> a -> [Char]
bmi' weight height
    | calculatedBmi < 18.5 = "Under weight"
    | calculatedBmi < 25.0 = "Normal"
    | calculatedBmi < 30.0 = "OK"
    | otherwise = "Concerning" 
    where calculatedBmi = weight / height ** 2