{-# LANGUAGE OverloadedStrings #-}

{-|
Checks allow users to post check statuses, for use with monitors.
-}
module Network.Datadog.Check
( CheckStatus(..)
, CheckResult(..)
, recordCheck
) where


import Control.Monad (void)

import Data.Aeson
import Data.Aeson.Types (modifyFailure, typeMismatch)
import Data.Text (Text, dropWhile, intercalate, tail, takeWhile)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.HTTP.Conduit

import Network.Datadog


-- | The status of a service, based on a check that is run against it.
data CheckStatus = CheckOk
                   -- ^ Everything is as it should be.
                 | CheckWarning
                   -- ^ Something abnormal, but not critical, is amiss.
                 | CheckCritical
                   -- ^ Something dangerously critical is amiss.
                 | CheckUnknown
                   -- ^ The current status cannot be determined.
                 deriving (Eq)

instance ToJSON CheckStatus where
  toJSON CheckOk = Number 0
  toJSON CheckWarning = Number 1
  toJSON CheckCritical = Number 2
  toJSON CheckUnknown = Number 3

instance FromJSON CheckStatus where
  parseJSON (Number 0) = return CheckOk
  parseJSON (Number 1) = return CheckWarning
  parseJSON (Number 2) = return CheckCritical
  parseJSON (Number 3) = return CheckUnknown
  parseJSON (Number n) = fail $ "CheckStatus: Number \"" ++ show n ++ "\" is not a valid CheckStatus"
  parseJSON a = modifyFailure ("MonitorType: " ++) $ typeMismatch "Number" a


-- | The result of running a check on some service.
data CheckResult = CheckResult { crCheck :: Text
                                 -- ^ Text describing the check
                               , crHostName :: Text
                                 -- ^ Name of the host which the check applies to
                               , crStatus :: CheckStatus
                                 -- ^ Status result of the check
                               , crTimestamp :: Maybe UTCTime
                                 -- ^ Time at which the check occurred (Nothing will wait until the
                                 -- check is sent to Datadog to compute the time)
                               , crMessage :: Maybe Text
                                 -- ^ Information related to why this specific check run supplied
                                 -- the status it did
                               , crTags :: [(Text,Text)]
                                 -- ^ (key,value) tags to associate with this check run
                               } deriving (Eq)

instance ToJSON CheckResult where
  toJSON cr = object (["check" .= crCheck cr
                      ,"host_name" .= crHostName cr
                      ,"status" .= (\(Number a) -> a) (toJSON (crStatus cr))
                      ,"tags" .= map (\(a,b) -> intercalate ":" [a,b]) (crTags cr)]
                      ++ maybe [] (\a -> ["timestamp" .= (floor (utcTimeToPOSIXSeconds a) :: Integer)]) (crTimestamp cr)
                      ++ maybe [] (\a -> ["message" .= a]) (crMessage cr))

instance FromJSON CheckResult where
  parseJSON (Object v) = modifyFailure ("CheckResult: " ++) $
                         CheckResult <$>
                         v .: "check" <*>
                         v .: "host_name" <*>
                         v .: "status" <*>
                         v .:? "timestamp" .!= Nothing <*>
                         v .:? "message" .!= Nothing <*>
                         (map keyvalue <$> (v .: "tags"))
    where keyvalue a = (Data.Text.takeWhile (/=':') a, Data.Text.tail (Data.Text.dropWhile (/=':') a))
  parseJSON a = modifyFailure ("CheckResult: " ++) $ typeMismatch "Object" a


-- | Record the result of a check in Datadog.
recordCheck :: Environment -> CheckResult -> IO ()
recordCheck (Environment keys manager) checkResult = do
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/check_run?api_key=" ++ apiKey keys
  let request = initReq { method = "POST"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode checkResult)
                        }
  void $ httpLbs request manager
