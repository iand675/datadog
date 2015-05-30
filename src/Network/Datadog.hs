{-|
<https://datadoghq.com Datadog> is a monitoring service for IT, Operations and
Development teams who write and run applications at scale, and want to turn the
massive amounts of data produced by their apps, tools and services into
actionable insight.
-}
module Network.Datadog
( Keys(..)
, loadKeysFromEnv
, Environment(..)
, createEnvironment
) where


import Network.HTTP.Conduit

import System.Environment (getEnv)


-- | Wraps the keys needed by Datadog to fully access the API.
data Keys = Keys { apiKey :: String
                   -- A write-key associated with a user
                 , appKey :: String
                   -- A read-key associated with an application
                 } deriving (Eq)

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


-- | An Environment contains everything needed to interact with Datadog.
data Environment = Environment { envKeys :: Keys
                                 -- ^ Auth keys to permit communication with Datadog
                               , envManager :: Manager
                                 -- ^ HTTP manager used to make requests to Datadog
                               }

-- | Create a new environment using authentication keys
createEnvironment :: Keys -> IO Environment
createEnvironment keys = fmap (Environment keys) managerIO
  where managerIO = newManager conduitManagerSettings
