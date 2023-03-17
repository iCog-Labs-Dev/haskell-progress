#!/usr/bin/env stack
-- stack script --resolver lts-12.21
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
import qualified Data.ByteString.Lazy.Char8 as L8
import           Network.HTTP.Simple
import Data.Aeson
import Data.Map as Map
import GHC.Generics

-- data TODO = TODO {
--     userId :: Int,
--     id :: Int,
--     title :: String,
--     completed :: Bool
--     } deriving (Generic, Show)
-- instance ToJSON TODO where
--     toEncoding = genericToEncoding defaultOptions
-- instance FromJSON TODO

data Location = Location {
    name :: String,
    country :: String
    } deriving (Generic, Show)

instance FromJSON Location where
    parseJSON (Object v) = Location <$> v .: "name" <*> v .: "country"
    -- parseJSON _ = empty

data Condition = Condition {
    text :: String
} deriving (Generic,Show)

instance FromJSON Condition where
    parseJSON (Object v) = Condition <$> v .: "text"
    -- parseJSON _ = empty

data Current = Current {
    temp_c :: Int,
    temp_f :: Double,
    condition :: Condition,
    last_updated :: String
    } deriving (Generic, Show)

instance FromJSON Current where
    parseJSON (Object v) = Current <$> v .: "temp_c" <*> v .: "temp_f" <*> v .: "condition" <*> v .: "last_updated"
    -- parseJSON _ = empty

data Day = Day {
    maxtemp_c :: Double,
    maxtemp_f :: Double,
    mintemp_c :: Double,
    mintemp_f :: Double,
    avgtemp_c :: Double,
    avgtemp_f :: Double
    } deriving (Generic, Show)

instance FromJSON Day where
    parseJSON (Object v) = Day <$> v .: "maxtemp_c" <*> v .: "maxtemp_f" <*> v .: "mintemp_c" <*> v .: "mintemp_f" <*> v .: "avgtemp_c" <*> v .: "avgtemp_f"
    -- parseJSON _ = empty

data Forecastday = Forecastday {
    day :: Day
} deriving (Show)
instance FromJSON Forecastday where
    parseJSON (Object v) = Forecastday <$> v .: "day"

newtype Forecast = Forecast [Forecastday] deriving Show

instance FromJSON Forecast where
    parseJSON (Object v) = Forecast <$> v .: "forecastday"
    -- parseJSON (Object o) = Forecast <$> o .: ""
    -- parseJSON _ = mzero

    -- parseJSON _ = empty

data Weather = Weather {
    current :: Current,
    location :: Location,
    forecast :: Forecast
    } deriving (Generic, Show)

instance FromJSON Weather where
    parseJSON (Object v) = Weather <$> v .: "current" <*> v .: "location" <*> v .: "forecast"
    -- parseJSON _ = empty

fromMaybeJSON :: Maybe a -> a
fromMaybeJSON Nothing = error "Nothing to show -> Network or JSON parsing error"
fromMaybeJSON (Just a) = a

city = name . location
region = country . location
getCelsius = show . temp_c . current 
getFahrenheit = show . temp_f . current
mood = text . condition . current
latestUpdate = last_updated . current

highTempC = show . maxtemp_c . day
highTempF = show . maxtemp_f . day
lowTempC = show .  mintemp_c . day
lowTempF = show .  mintemp_f . day


displayDayForecast :: Forecastday -> IO ()
displayDayForecast fore = do 
    putStrLn $ highTempC fore

displayForecast :: [Forecastday] -> IO ()
displayForecast [] = return ()
displayForecast (x:xs) = displayDayForecast x

fetchForecast n = do 
    response <- httpLBS ("http://api.weatherapi.com/v1/forecast.json?key=11ca9ac0925945ab8ea93253231703&q=Addis ababa&days=2&aqi=no&alerts=no")
    let res = L8.unpack $ getResponseBody response
        result =  fromMaybeJSON (decode (getResponseBody response) :: Maybe Weather)
    putStrLn $ res
    -- displayForecast (forecast result)

fetchWeatherDataForTodayAndPrint :: IO ()
fetchWeatherDataForTodayAndPrint = do 
    response <- httpLBS "http://api.weatherapi.com/v1/forecast.json?key=11ca9ac0925945ab8ea93253231703&q=Addis ababa&days=2&aqi=no&alerts=no"
    let res = L8.unpack $ getResponseBody response
        result =  fromMaybeJSON (decode (getResponseBody response) :: Maybe Weather)
    putStrLn res
    print $ result
    -- putStrLn ""
    putStrLn $ replicate 50 '-'
    putStrLn $ "Weather information for today"
    putStrLn $ replicate 50 '='
    putStrLn $ "Location: " ++ city result ++ ", " ++ region result
    putStrLn $ "Temperature: " ++ getCelsius result ++ "°C / " ++ getFahrenheit result ++ "°F"
    putStrLn $ "Condition: " ++ mood result
    putStrLn ""
    putStrLn $ "Recent update " ++ latestUpdate result ++ " from weatherapi.com"
    putStrLn $ replicate 50 '-'

toInt :: String -> Int
toInt a = read a :: Int
-- huh

loop :: IO ()
loop = do 
    putStrLn "Enter a number: "
    n <- getLine
    if  n == "1" then do
        fetchWeatherDataForTodayAndPrint
    else do
        fetchForecast n
    -- loop
main :: IO ()
main = do
    putStrLn $ replicate 50 '='
    putStrLn "MENU for Weather app"
    putStrLn ""
    putStrLn $ "1. Today's temperature"
    putStrLn $ "2+. Get forecast for days of numbers" 
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