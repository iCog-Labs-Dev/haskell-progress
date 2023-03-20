{-# LANGUAGE OverloadedStrings #-}

module Skeleton where 

import Data.Aeson

data Location = Location
  { name :: String,
    country :: String
  }
  deriving (Show)

instance FromJSON Location where
  parseJSON (Object v) = Location <$> v .: "name" <*> v .: "country"

newtype Condition = Condition
  { text :: String
  }
  deriving (Show)

instance FromJSON Condition where
  parseJSON (Object v) = Condition <$> v .: "text"


data Current = Current
  { temp_c :: Double,
    temp_f :: Double,
    condition :: Condition,
    last_updated :: String
  }
  deriving (Show)

instance FromJSON Current where
  parseJSON (Object v) = Current <$> v .: "temp_c" <*> v .: "temp_f" <*> v .: "condition" <*> v .: "last_updated"


data Day = Day
  { maxtemp_c :: Double,
    maxtemp_f :: Double,
    mintemp_c :: Double,
    mintemp_f :: Double,
    avgtemp_c :: Double,
    avgtemp_f :: Double,
    conditionForecast :: Condition
  }
  deriving (Show)

instance FromJSON Day where
  parseJSON (Object v) = Day <$> v .: "maxtemp_c" <*> v .: "maxtemp_f" <*> v .: "mintemp_c" <*> v .: "mintemp_f" <*> v .: "avgtemp_c" <*> v .: "avgtemp_f" <*> v .: "condition"

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

data Weather = Weather
  { current :: Current,
    location :: Location,
    forecast :: Forecast
  }
  deriving (Show)

instance FromJSON Weather where
  parseJSON (Object v) = Weather <$> v .: "current" <*> v .: "location" <*> v .: "forecast"
