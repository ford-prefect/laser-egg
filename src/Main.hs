{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Lens ((^.), (.~), (&))
import Data.Aeson (FromJSON, Value(..), (.:), parseJSON)
import Data.Text (Text, pack)
import GHC.Generics (Generic)
import Network.Wreq (asJSON, defaults, getWith, param, responseBody)
import System.Environment (getEnv)

data Config = Config { apiEndpoint :: String
                     , apiKey      :: String
                     , deviceUUID  :: String
                     , pollRate    :: Int -- Seconds
                     }

data Data = Data { pm25     :: Double -- µg/m3
                 , pm10     :: Double -- µg/m3
                 , humidity :: Double -- %age
                 , temp     :: Double -- °C
                 , rtvoc    :: Double -- parts per billion
                 } deriving (Generic, Show)

data Reading = Reading { ts    :: Text   -- RFC3339 time
                       , value :: Data
                       } deriving (Generic, Show)

data LaserEgg = LaserEgg { uuid :: Text -- UUID
                         , info :: Reading
                         } deriving (Generic, Show)

instance FromJSON Data

instance FromJSON Reading where
  parseJSON (Object v) = Reading
    <$> v .: "ts"
    <*> v .: "data"

instance FromJSON LaserEgg where
  parseJSON (Object v) = LaserEgg
    <$> v .: "id"
    <*> v .: "info.aqi"

defaultEndpoint :: String
defaultEndpoint = "https://api.origins-china.cn/v1"

defaultPollRate :: Int
defaultPollRate = 60

defaultConfig :: IO Config
defaultConfig = Config <$> pure defaultEndpoint
                       <*> getEnv "KAITERRA_API_KEY"
                       <*> getEnv "KAITERRA_DEVICE_UUID"
                       <*> pure defaultPollRate

main :: IO ()
main = do
  config <- defaultConfig
  let url     = apiEndpoint config ++ "/lasereggs/" ++ deviceUUID config
      options = defaults & param "key" .~ [pack $ apiKey config]
  -- Now get one set of readings
  resp <- asJSON =<< getWith options url
  print $ info $ resp ^. responseBody
