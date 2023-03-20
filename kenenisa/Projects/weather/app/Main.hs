
-- stack script --resolver lts-12.21

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import Data.Map as Map
import GHC.Generics
import Network.HTTP.Simple
import Data.Char (isDigit)

-- data TODO = TODO {
--     userId :: Int,
--     id :: Int,
--     title :: String,
--     completed :: Bool
--     } deriving (Generic, Show)
-- instance ToJSON TODO where
--     toEncoding = genericToEncoding defaultOptions
-- instance FromJSON TODO

data Location = Location
  { name :: String,
    country :: String
  }
  deriving (Generic, Show)

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "name" <*> v .: "country"

-- parseJSON _ = empty

newtype Condition = Condition
  { text :: String
  }
  deriving (Generic, Show)

instance FromJSON Condition where
  parseJSON (Object v) = Condition <$> v .: "text"

-- parseJSON _ = empty

data Current = Current
  { temp_c :: Double,
    temp_f :: Double,
    condition :: Condition,
    last_updated :: String
  }
  deriving (Generic, Show)

instance FromJSON Current where
  parseJSON (Object v) = Current <$> v .: "temp_c" <*> v .: "temp_f" <*> v .: "condition" <*> v .: "last_updated"

-- parseJSON _ = empty

data Day = Day
  { maxtemp_c :: Double,
    maxtemp_f :: Double,
    mintemp_c :: Double,
    mintemp_f :: Double,
    avgtemp_c :: Double,
    avgtemp_f :: Double,
    conditionForecast :: Condition
  }
  deriving (Generic, Show)

instance FromJSON Day where
  parseJSON (Object v) = Day <$> v .: "maxtemp_c" <*> v .: "maxtemp_f" <*> v .: "mintemp_c" <*> v .: "mintemp_f" <*> v .: "avgtemp_c" <*> v .: "avgtemp_f" <*> v .: "condition"

-- parseJSON _ = empty

data Forecastday = Forecastday
  {
    day :: Day,
    date :: String
  }
  deriving (Show)

instance FromJSON Forecastday where
  parseJSON (Object v) = Forecastday <$> v .: "day" <*> v .: "date"

newtype Forecast = Forecast [Forecastday] deriving (Show)

instance FromJSON Forecast where
  parseJSON (Object v) = Forecast <$> v .: "forecastday"

-- parseJSON (Object o) = Forecast <$> o .: ""
-- parseJSON _ = mzero

-- parseJSON _ = empty

data Weather = Weather
  { current :: Current,
    location :: Location,
    forecast :: Forecast
  }
  deriving (Generic, Show)

instance FromJSON Weather where
  parseJSON (Object v) = Weather <$> v .: "current" <*> v .: "location" <*> v .: "forecast"

-- parseJSON _ = empty

fromMaybeJSON :: Maybe a -> a
fromMaybeJSON Nothing = error "Nothing to show -> Network or JSON parsing error"
fromMaybeJSON (Just a) = a

city :: Weather -> String
city = name . location

region :: Weather -> String
region = country . location

getCelsius :: Weather -> String
getCelsius = show . temp_c . current

getFahrenheit :: Weather -> String
getFahrenheit = show . temp_f . current

mood :: Weather -> String
mood = text . condition . current

latestUpdate :: Weather -> String
latestUpdate = last_updated . current

highTempC :: Forecastday -> String
highTempC = show . maxtemp_c . day

highTempF :: Forecastday -> String
highTempF = show . maxtemp_f . day

lowTempC :: Forecastday -> String
lowTempC = show . mintemp_c . day

lowTempF :: Forecastday -> String
lowTempF = show . mintemp_f . day

averageTempC :: Forecastday -> String
averageTempC = show . avgtemp_c . day

averageTempF :: Forecastday -> String
averageTempF = show . avgtemp_f . day

forecastCondition :: Forecastday -> String
forecastCondition = text . conditionForecast . day

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

-- putStrLn ""

toInt :: String -> Int
toInt a = read a :: Int

-- huh
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

-- main = do
--     putStrLn $ show $ fromMaybeJSON $ (decode "{\"total\":1,\"forecastday\":[ {\"id\":\"771315522\",\"title\":\"Harry Potter and the Philosophers Stone (Wizard's Collection)\"}]}" :: Maybe Forecast)

-- data day = day { id :: String, title :: String } deriving (Show)

-- newtype Forecast = Forecast [day] deriving (Show)

-- instance FromJSON Forecast where
--   parseJSON (Object o) =
--     Forecast <$> (o .: "days")

-- instance FromJSON Day where
--   parseJSON (Object o) =
--     Day <$> (o .: "id")
--           <*> (o .: "title")