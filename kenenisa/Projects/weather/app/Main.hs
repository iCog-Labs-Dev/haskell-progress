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
instance ToJSON Location where
    toJSON (Location name country) = object ["name" .= name, "country" .= country]

data Condition = Condition {
    text :: String
} deriving (Generic,Show)

instance FromJSON Condition where
    parseJSON (Object v) = Condition <$> v .: "text"
    -- parseJSON _ = empty
instance ToJSON Condition where
    toJSON (Condition text) = object ["text" .= text]

data Current = Current {
    temp_c :: Int,
    temp_f :: Double,
    condition :: Condition,
    last_updated :: String
    } deriving (Generic, Show)

instance FromJSON Current where
    parseJSON (Object v) = Current <$> v .: "temp_c" <*> v .: "temp_f" <*> v .: "condition" <*> v .: "last_updated"
    -- parseJSON _ = empty
instance ToJSON Current where
    toJSON (Current temp_c temp_f condition last_updated) = object ["temp_c" .= temp_c, "temp_f" .= temp_f, "condition" .= condition,"last_updated" .= last_updated]

data Weather = Weather {
    current :: Current,
    location :: Location
    } deriving (Generic, Show)

instance FromJSON Weather where
    parseJSON (Object v) = Weather <$> v .: "current" <*> v .: "location"
    -- parseJSON _ = empty
instance ToJSON Weather where
    toJSON (Weather current location) = object ["current" .= current, "location" .= location]

fromMaybeJSON :: Maybe Weather -> Weather
fromMaybeJSON Nothing = error "Nothing to show -> Network or JSON parsing error"
fromMaybeJSON (Just a) = a

city = name . location
region = country . location
getCelsius = show . temp_c . current 
getFahrenheit = show . temp_f . current
mood = text . condition . current
latestUpdate = last_updated . current

fetchWeatherData :: IO ()
fetchWeatherData = do 
    response <- httpLBS "http://api.weatherapi.com/v1/current.json?key=11ca9ac0925945ab8ea93253231703&q=Addis ababa&aqi=no"
    let res = L8.unpack $ getResponseBody response
        result =  fromMaybeJSON (decode (getResponseBody response) :: Maybe Weather)
    putStrLn ""
    putStrLn $ replicate 50 '-'
    putStrLn $ "Weather information for today"
    putStrLn $ replicate 50 '='
    putStrLn $ "Location: " ++ city result ++ ", " ++ region result
    putStrLn $ "Temperature: " ++ getCelsius result ++ "°C / " ++ getFahrenheit result ++ "°F"
    putStrLn $ "Condition: " ++ mood result
    putStrLn ""
    putStrLn $ "Recent update " ++ latestUpdate result ++ " from weatherapi.com"
    putStrLn $ replicate 50 '-'


-- huh
main :: IO ()
main = do
    putStrLn "MENU for Weather app"
    fetchWeatherData