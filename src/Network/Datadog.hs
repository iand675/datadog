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
, Tag
) where


import Data.Aeson
import Data.Aeson.Types (modifyFailure, typeMismatch)
import qualified Data.Text as T

import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)

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
                               , envAPIUrl :: String
                                 -- ^ The root URL for the Datadog API
                               , envManager :: Manager
                                 -- ^ HTTP manager used to make requests to Datadog
                               }

-- | Create a new environment using authentication keys, defaulting to the
-- Datadog documented default API URL.
createEnvironment :: Keys -> IO Environment
createEnvironment keys = fmap (Environment keys "https://app.datadoghq.com/api/v1/") managerIO
  where managerIO = newManager tlsManagerSettings


-- | Entity descriptor.
--
-- Entities in Datadog (hosts, metrics, events, etc) are frequently associated
-- with one more more "tags". These tags are labels that identify an entity as
-- belonging to a particular group or having particular properties. A tag can
-- come in two forms: a simple text label, describing entities associated with
-- the tag, or a key-value pair, associating entities with a specific slice of
-- a larger categorization.
--
-- As strings, the key and value parts of a key-value pair are separated by a
-- (':'). As such, any tag with no colons is a label, and any tag with one (or
-- more) is a key-value pair - if more than one ':' is specified, the
-- additional ':'s will become part of the value.
data Tag = KeyValueTag T.Text T.Text
         | LabelTag T.Text
         deriving (Eq)

instance Show Tag where
  show (KeyValueTag k v) = T.unpack k ++ (':' : T.unpack v)
  show (LabelTag t) = T.unpack t

instance Read Tag where
  readsPrec _ s = let t = T.pack s
                  in (\a -> [(a, "")]) $
                     maybe (LabelTag t) (\i -> uncurry KeyValueTag (T.splitAt i t)) $
                     T.findIndex (==':') t

instance ToJSON Tag where
  toJSON (KeyValueTag k v) = Data.Aeson.String $ k `T.append` (':' `T.cons` v)
  toJSON (LabelTag t) = Data.Aeson.String t

instance FromJSON Tag where
  parseJSON (String s) = return $
                         maybe (LabelTag s) (\i -> uncurry KeyValueTag (T.splitAt i s)) $
                         T.findIndex (==':') s
  parseJSON a = modifyFailure ("Tag: " ++) $ typeMismatch "String" a
