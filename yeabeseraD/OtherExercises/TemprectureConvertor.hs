main = do 
  putStrLn "Enter your temprature: "
  input <- getLine

  let temprature = read input + 0.0

  putStrLn "Input 'C' for celisous or 'F' for farenhight: '"
  choice <- getLine

  let convertedTemprature = case choice of "C" -> toCelcius temprature
                                           "F" -> toFarenhieght temprature
                                           _ -> error "Invalid input"
  let message = case choice of "C"->"From farenheit to celisus"
                               "F"->"From celisus to farenhiet"
  putStrLn (" Converted temprature "++message++" is " ++ show convertedTemprature)

toFarenhieght t = (9/5) * t + 32
toCelcius t = (5/9)*(t-32)
