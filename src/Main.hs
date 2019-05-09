{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (threadDelay)
import Control.Exception (SomeException(..), try)
import Control.Lens ((^.), (.~), (&))
import Data.Aeson (FromJSON, Value(..), (.:), parseJSON)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.Wreq (asJSON, defaults, getWith, param, responseBody)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)

data Config = Config { apiEndpoint :: String
                     , apiKey      :: String
                     , deviceUUID  :: String
                     , pollRate    :: Int -- Seconds
                     , outFile     :: FilePath
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

defaultFile :: FilePath
defaultFile = "aqi.txt"

defaultConfig :: IO Config
defaultConfig = Config <$> pure defaultEndpoint
                       <*> getEnv "KAITERRA_API_KEY"
                       <*> getEnv "KAITERRA_DEVICE_UUID"
                       <*> pure defaultPollRate
                       <*> pure defaultFile

writeHeader :: FilePath -> IO ()
writeHeader file = do
  let header = "# time, pm25, pm10, humidity, temp, rtvoc\n"
  appendFile file header

writeData :: FilePath -> Reading -> IO ()
writeData file (Reading ts (Data pm25 pm10 hum temp rtvoc)) = do
  let values = unpack ts : map show [pm25, pm10, hum, temp, rtvoc]
      out    = intercalate ", " values ++ "\n"
  appendFile file out

main :: IO ()
main = do
  config <- defaultConfig

  -- Write a header to start with
  writeHeader $ outFile config

  -- Now read data, write it out, and sleep in a loop
  runLoop config

  where
    runLoop config = do
      let url     = apiEndpoint config ++ "/lasereggs/" ++ deviceUUID config
          options = defaults & param "key" .~ [pack $ apiKey config]

      res <- try $ do
          -- Call the API
          resp <- asJSON =<< getWith options url
          let reading = info $ resp ^. responseBody

          -- Write out the data
          writeData (outFile config) reading

      case res of
           Left err -> hPutStrLn stderr $ "Error: " ++ show (err :: SomeException)
           Right _  -> pure ()

      -- Wait for a bit
      threadDelay $ 1000000 * pollRate config

      runLoop config
