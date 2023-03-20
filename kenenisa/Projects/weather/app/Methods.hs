module Methods where

import Skeleton

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