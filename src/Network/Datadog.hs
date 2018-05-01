{-|
<https://datadoghq.com Datadog> is a monitoring service for IT, Operations and
Development teams who write and run applications at scale, and want to turn the
massive amounts of data produced by their apps, tools and services into
actionable insight.
-}
module Network.Datadog
( Keys(..)
, loadKeysFromEnv
, Environment
, createEnvironment
, withDatadog
, writeCredentials
, readWriteCredentials
, module Network.Datadog.Check
, module Network.Datadog.Downtime
, module Network.Datadog.Event
, module Network.Datadog.Host
, module Network.Datadog.Metrics
, module Network.Datadog.Monitor
) where


import qualified Data.Text as T
import           Data.Text.Encoding (encodeUtf8)

import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

import System.Environment (getEnv)

import Network.Datadog.Check
import Network.Datadog.Downtime
import Network.Datadog.Event
import Network.Datadog.Host
import Network.Datadog.Metrics
import Network.Datadog.Monitor
import Network.Datadog.Internal

-- | Load Datadog keys from environment variables.
--
-- The keys will be read from the enviornment variables
-- `DATADOG_API_KEY` and `DATADOG_APP_KEY`. If the keys cannot be read, this
-- function will throw an 'IOException'.
loadKeysFromEnv :: IO Keys
loadKeysFromEnv = do
  api <- getEnv "DATADOG_API_KEY"
  app <- getEnv "DATADOG_APP_KEY"
  return $ Keys api app

-- | Create a new environment using authentication keys, defaulting to the
-- Datadog documented default API URL.
createEnvironment :: Keys -> IO Environment
createEnvironment keys = fmap (Environment keys "https://api.datadoghq.com/api/v1/") managerIO
  where managerIO = newManager tlsManagerSettings

withDatadog :: DatadogCredentials k => k -> (DatadogClient k -> IO a) -> IO a
withDatadog k f = newManager tlsManagerSettings >>= \man -> f $ DatadogClient man k

writeCredentials :: T.Text -> Write
writeCredentials = Write . encodeUtf8

readWriteCredentials :: T.Text -> T.Text -> ReadWrite
readWriteCredentials r w = ReadWrite (encodeUtf8 w) (encodeUtf8 r)

