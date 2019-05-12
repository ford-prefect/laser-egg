{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Concurrent (forkIO, threadDelay)
import Control.Exception (SomeException(..), try)
import Control.Lens ((^.), (.~), (&))
import Data.Aeson (FromJSON, Value(..), (.:), parseJSON)
import Data.List (intercalate)
import Data.Text (Text, pack, unpack)
import GHC.Generics (Generic)
import Network.HTTP.Types (hContentType, status200, status404)
import Network.Wai (pathInfo, responseFile, responseLBS, requestMethod)
import Network.Wai.Handler.Warp (run)
import Network.Wai.Middleware.Prometheus (def, prometheus)
import Network.Wreq (asJSON, defaults, getWith, param, responseBody)
import Prometheus (Gauge, Info(..), Metric, gauge, register, setGauge)
import System.Environment (getEnv)
import System.IO (hPutStrLn, stderr)

data Config = Config { apiEndpoint :: String
                     , apiKey      :: String
                     , deviceUUID  :: String
                     , pollRate    :: Int -- Seconds
                     , outFile     :: FilePath
                     , httpPort    :: Int
                     }

data Gauges = Gauges { gPm25     :: Gauge
                     , gPm10     :: Gauge
                     , gHumidity :: Gauge
                     , gTemp     :: Gauge
                     , gRtvoc    :: Gauge
                     }

data Data = Data { pm25     :: Double -- µg/m3
                 , pm10     :: Double -- µg/m3
                 , humidity :: Double -- %age
                 , temp     :: Double -- °C
                 , rtvoc    :: Maybe Double -- parts per billion
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

defaultHttpPort :: Int
defaultHttpPort = 10000

defaultConfig :: IO Config
defaultConfig = Config <$> pure defaultEndpoint
                       <*> getEnv "KAITERRA_API_KEY"
                       <*> getEnv "KAITERRA_DEVICE_UUID"
                       <*> pure defaultPollRate
                       <*> pure defaultFile
                       <*> pure defaultHttpPort

writeHeader :: FilePath -> IO ()
writeHeader file = do
  let header = "# time, pm25, pm10, humidity, temp, rtvoc\n"
  appendFile file header

writeData :: FilePath -> Reading -> IO ()
writeData file (Reading ts (Data pm25 pm10 hum temp rtvoc)) = do
  let values = (unpack ts : map show [pm25, pm10, hum, temp]) ++ [maybe "" show rtvoc]
      out    = intercalate ", " values ++ "\n"
  appendFile file out

initGauges :: IO Gauges
initGauges = Gauges <$> register (gauge $ Info "pm25" "PM2.5 (µg/m3)")
                    <*> register (gauge $ Info "pm10" "PM10 (µg/m3)")
                    <*> register (gauge $ Info "humidity" "Humidity (%age)")
                    <*> register (gauge $ Info "temperature" "Celsius")
                    <*> register (gauge $ Info "tvoc" "Parts per billion)")

main :: IO ()
main = do
  config <- defaultConfig
  gauges <- initGauges

  -- Write a header to start with
  writeHeader $ outFile config

  -- Now read data, write it out, and sleep in a loop
  forkIO $ runLoop config gauges

  -- Start the HTTP server for Prometheus metrics
  run (httpPort config) (prometheus def $ app config)

  where
    app config request respond = respond $
      if requestMethod request == "GET" &&
         pathInfo request == [pack $ deviceUUID config]
      then
        -- Serve up all the data we have collected in the file
        responseFile status200 [(hContentType, "text/plain")] (outFile config) Nothing
      else
        -- 404
        responseLBS status404 [] ""

    runLoop config gauges = do
      let url     = apiEndpoint config ++ "/lasereggs/" ++ deviceUUID config
          options = defaults & param "key" .~ [pack $ apiKey config]

      res <- try $ do
          -- Call the API
          resp <- asJSON =<< getWith options url
          let reading = info $ resp ^. responseBody

          -- Write out the data
          writeData (outFile config) reading

          -- Expose metrics to Prometheus
          setGauge (gPm25 gauges) (pm25 $ value reading)
          setGauge (gPm10 gauges) (pm10 $ value reading)
          setGauge (gHumidity gauges) (humidity $ value reading)
          setGauge (gTemp gauges) (temp $ value reading)
          sequence $ setGauge (gRtvoc gauges) <$> rtvoc (value reading)

      case res of
           Left err -> hPutStrLn stderr $ "Error: " ++ show (err :: SomeException)
           Right _  -> pure ()

      -- Wait for a bit
      threadDelay $ 1000000 * pollRate config

      runLoop config gauges
