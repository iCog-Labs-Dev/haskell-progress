
-- stack script --resolver lts-12.21

{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson

import qualified Data.ByteString.Lazy.Char8 as L8

import Network.HTTP.Simple
import Data.Char (isDigit)

-- My modules
import Skeleton
import Methods

displayDayForecast :: Forecastday -> IO ()
displayDayForecast fore = do
    putStrLn $ '\n' : replicate 50 '='
    putStrLn $ "|| Forecast for " ++ date fore
    putStrLn $ replicate 50 '='
    putStrLn $ "Highest: " ++ highTempC fore ++ "°C / " ++ highTempF fore ++ "°F"
    putStrLn $ "Lowest: " ++ lowTempC fore ++ "°C / " ++ lowTempF fore ++ "°F"
    putStrLn $ "Average: " ++ averageTempC fore ++ "°C / " ++ averageTempF fore ++ "°F"
    putStrLn $ "Condition: " ++ forecastCondition fore
    putStrLn $ replicate 50 '-'


displayForecast :: Forecast -> IO ()
displayForecast (Forecast []) = return ()
displayForecast (Forecast (x : xs)) = do
    displayDayForecast x
    displayForecast (Forecast xs)

constructUrl :: String -> String
constructUrl n = "http://api.weatherapi.com/v1/forecast.json?key=11ca9ac0925945ab8ea93253231703&q=Addis ababa&days="++n++"&aqi=no&alerts=no"

fetchForecast :: String -> IO ()
fetchForecast n = do
  response <- httpLBS $ parseRequest_ (constructUrl n)
  let res = L8.unpack $ getResponseBody response
      result = fromMaybeJSON (decode (getResponseBody response) :: Maybe Weather)
  printTodaysWeather result
  displayForecast (forecast result)

printTodaysWeather :: Weather -> IO ()
printTodaysWeather result = do
  putStrLn $ replicate 50 '='
  putStrLn "Weather information for today"
  putStrLn $ replicate 50 '='
  putStrLn $ "Location: " ++ city result ++ ", " ++ region result
  putStrLn $ "Temperature: " ++ getCelsius result ++ "°C / " ++ getFahrenheit result ++ "°F"
  putStrLn $ "Condition: " ++ mood result
  putStrLn ""
  putStrLn $ "Recent update " ++ latestUpdate result ++ " from weatherapi.com"
  putStrLn $ replicate 50 '-'

fetchWeatherDataForTodayAndPrint :: IO ()
fetchWeatherDataForTodayAndPrint = do
  response <- httpLBS "http://api.weatherapi.com/v1/forecast.json?key=11ca9ac0925945ab8ea93253231703&q=Addis ababa&days=2&aqi=no&alerts=no"
  let res = L8.unpack $ getResponseBody response
      result = fromMaybeJSON (decode (getResponseBody response) :: Maybe Weather)
  printTodaysWeather result

toInt :: String -> Int
toInt a = read a :: Int

checkDigits :: String -> Bool
checkDigits = Prelude.foldr ((&&) . isDigit) True

loop :: IO ()
loop = do
  putStrLn "Enter a number: "
  n <- getLine
  if n == "1"
    then do
      fetchWeatherDataForTodayAndPrint
    else if checkDigits n then do
            fetchForecast n
        else do 
            putStrLn "Sorry, You can only enter numbers" 

-- loop
main :: IO ()
main = do
  putStrLn $ replicate 50 '='
  putStrLn "MENU for Weather app"
  putStrLn "1. Today's temperature"
  putStrLn "2+. Get forecast for number of days"
  loop