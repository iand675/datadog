{-# LANGUAGE OverloadedStrings #-}
-- we infect all the other modules with instances from
-- this module, so they don't appear orphaned.
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Network.Datadog.Internal
( prependMaybe
, prependBool
, datadogHttp
, decodeDatadog
, baseRequest
, defaultMonitorOptions
, DatadogCredentials(..)
, module Network.Datadog.Lens
, module Network.Datadog.Types
) where

import Control.Arrow (first)
import Control.Exception
import Control.Lens hiding ((.=), cons)

import Data.Aeson hiding (Series, Success, Error)
import Data.Aeson.Types (modifyFailure, typeMismatch)
import qualified Data.ByteString.Lazy as LBS (ByteString, empty)
import qualified Data.DList as D
import qualified Data.HashMap.Strict as HM
import Data.Maybe
import Data.Text (Text, pack, append, splitAt, findIndex, cons)
import Data.Text.Lazy (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector ((!?))

import Network.HTTP.Client hiding (host)
import Network.HTTP.Types

import Network.Datadog.Types
import Network.Datadog.Lens
import Prelude hiding (splitAt)

prependMaybe :: (a -> b) -> Maybe a -> [b] -> [b]
prependMaybe f = maybe id ((:) . f)


prependBool :: Bool -> b -> [b] -> [b]
prependBool p a = if p then (a :) else id


datadogHttp :: Environment-> String -> [(String, String)] -> StdMethod -> Maybe LBS.ByteString -> IO LBS.ByteString
datadogHttp (Environment keys baseUrl manager) endpoint q httpMethod content = do
  initReq <- parseUrlThrow $ baseUrl ++ endpoint
  let body = RequestBodyLBS $ fromMaybe LBS.empty content
      headers = [("Content-type", "application/json") | isJust content]
      apiQuery = [("api_key", apiKey keys)
                 ,("application_key", appKey keys)]
      fullQuery = map (\(a,b) -> (encodeUtf8 (pack a), Just (encodeUtf8 (pack b)))) $
                  apiQuery ++ q
      request = setQueryString fullQuery $
                initReq { method = renderStdMethod httpMethod
                        , requestBody = body
                        , requestHeaders = headers
                        }
  responseBody <$> httpLbs request manager


decodeDatadog :: FromJSON a => String -> LBS.ByteString -> IO a
decodeDatadog funcname body = either (throwIO . AssertionFailed . failstring) return $
                              eitherDecode body
  where failstring e = "Datadog Library decoding failure in \"" ++ funcname ++
                       "\": " ++ e ++ ": " ++ unpack (decodeUtf8 body)

baseRequest :: Request
baseRequest = fromJust $ parseUrlThrow "https://api.datadoghq.com"

class DatadogCredentials s where
  signRequest :: s -> Request -> Request

instance DatadogCredentials Write where
  signRequest (Write k) = setQueryString [("api_key", Just k)]

instance DatadogCredentials ReadWrite where
  signRequest (ReadWrite w r) = setQueryString [("api_key", Just w), ("application_key", Just r)]

instance ToJSON DowntimeSpec where
  toJSON ds = object $
              prependMaybe (\a -> "start" .= (ceiling (utcTimeToPOSIXSeconds a) :: Integer)) (ds ^. start) $
              prependMaybe (\a -> "end" .= (floor (utcTimeToPOSIXSeconds a) :: Integer)) (ds ^. end)
              ["scope" .= (ds ^. scope)]

instance FromJSON DowntimeSpec where
  parseJSON (Object v) = modifyFailure ("DowntimeSpec: " ++) $
                         DowntimeSpec <$>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "start")) <*>
                         (maybe (return Nothing) (withScientific "Integer" (\t -> return (Just (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))))) =<< (v .:? "end")) <*>
                         v .:? "message" .!= Nothing <*>
                         (withArray "Text" (\t -> maybe (fail "\"scope\" Array is too short") parseJSON (t !? 0)) =<< v .: "scope")
  parseJSON a = modifyFailure ("DowntimeSpec: " ++) $ typeMismatch "Object" a

instance ToJSON Tag where
  toJSON (KeyValueTag k v) = Data.Aeson.String $ k `append` (':' `cons` v)
  toJSON (LabelTag t) = Data.Aeson.String t

instance FromJSON Tag where
  parseJSON (String s) = return $
                         maybe (LabelTag s) (\i -> uncurry KeyValueTag (splitAt i s)) $
                         findIndex (==':') s
  parseJSON a = modifyFailure ("Tag: " ++) $ typeMismatch "String" a

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

instance ToJSON CheckResult where
  toJSON cr = object $
              prependMaybe (\a -> "timestamp" .= (floor (utcTimeToPOSIXSeconds a) :: Integer)) (cr ^. timestamp) $
              prependMaybe (\a -> "message" .= a) (cr ^. message)
              ["check" .= (cr ^. check)
              ,"host_name" .= (cr ^. hostName)
              ,"status" .= (cr ^. status)
              ,"tags" .= (cr ^. tags)
              ]

instance FromJSON CheckResult where
  parseJSON (Object v) = modifyFailure ("CheckResult: " ++) $
                         CheckResult <$>
                         v .: "check" <*>
                         v .: "host_name" <*>
                         v .: "status" <*>
                         v .:? "timestamp" .!= Nothing <*>
                         v .:? "message" .!= Nothing <*>
                         v .: "tags" .!= []
  parseJSON a = modifyFailure ("CheckResult: " ++) $ typeMismatch "Object" a

instance ToJSON Downtime where
  toJSON downtime = Object $ HM.insert "id" (toJSON $ downtime ^. id') basemap
    where (Object basemap) = toJSON (downtime ^. spec)

instance FromJSON Downtime where
  parseJSON (Object v) = modifyFailure ("Downtime: " ++) $
                         Downtime <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Downtime: " ++) $ typeMismatch "Object" a

instance ToJSON EventPriority where
  toJSON NormalPriority = Data.Aeson.String "normal"
  toJSON LowPriority = Data.Aeson.String "low"

instance FromJSON EventPriority where
  parseJSON (Data.Aeson.String "normal") = return NormalPriority
  parseJSON (Data.Aeson.String "low") = return LowPriority
  parseJSON (Data.Aeson.String s) = fail $ "EventPriority: String " ++ show s ++ " is not a valid EventPriority"
  parseJSON a = modifyFailure ("EventPriority: " ++) $ typeMismatch "String" a

instance ToJSON AlertType where
  toJSON Error = Data.Aeson.String "error"
  toJSON Warning = Data.Aeson.String "warning"
  toJSON Info = Data.Aeson.String "info"
  toJSON Success = Data.Aeson.String "success"

instance FromJSON AlertType where
  parseJSON (Data.Aeson.String "error") = return Error
  parseJSON (Data.Aeson.String "warning") = return Warning
  parseJSON (Data.Aeson.String "info") = return Info
  parseJSON (Data.Aeson.String "success") = return Success
  parseJSON (Data.Aeson.String s) = fail $ "AlertType: String " ++ show s ++ " is not a valid AlertType"
  parseJSON a = modifyFailure ("AlertType: " ++) $ typeMismatch "String" a

instance ToJSON SourceType where
  toJSON Nagios = Data.Aeson.String "nagios"
  toJSON Hudson = Data.Aeson.String "hudson"
  toJSON Jenkins = Data.Aeson.String "jenkins"
  toJSON User = Data.Aeson.String "user"
  toJSON MyApps = Data.Aeson.String "my apps"
  toJSON Feed = Data.Aeson.String "feed"
  toJSON Chef = Data.Aeson.String "chef"
  toJSON Puppet = Data.Aeson.String "puppet"
  toJSON Git = Data.Aeson.String "git"
  toJSON BitBucket = Data.Aeson.String "bitbucket"
  toJSON Fabric = Data.Aeson.String "fabric"
  toJSON Capistrano = Data.Aeson.String "capistrano"

instance FromJSON SourceType where
  parseJSON (Data.Aeson.String "nagios") = return Nagios
  parseJSON (Data.Aeson.String "hudson") = return Hudson
  parseJSON (Data.Aeson.String "jenkins") = return Jenkins
  parseJSON (Data.Aeson.String "user") = return User
  parseJSON (Data.Aeson.String "my apps") = return MyApps
  parseJSON (Data.Aeson.String "feed") = return Feed
  parseJSON (Data.Aeson.String "chef") = return Chef
  parseJSON (Data.Aeson.String "puppet") = return Puppet
  parseJSON (Data.Aeson.String "git") = return Git
  parseJSON (Data.Aeson.String "bitbucket") = return BitBucket
  parseJSON (Data.Aeson.String "fabric") = return Fabric
  parseJSON (Data.Aeson.String "capistrano") = return Capistrano
  parseJSON (Data.Aeson.String s) = fail $ "SourceType: String " ++ show s ++ " is not a valid SourceType"
  parseJSON a = modifyFailure ("SourceType: " ++) $ typeMismatch "String" a

instance ToJSON EventSpec where
  toJSON ed = object $
              prependMaybe (\a -> "host" .= a) (ed ^. host) $
              prependMaybe (\a -> "source_type_name" .= pack (show a)) (ed ^. sourceType)
              ["title" .= (ed ^. title)
              ,"text" .= (ed ^. text)
              ,"date_happened" .= (floor (utcTimeToPOSIXSeconds (ed ^. dateHappened)) :: Integer)
              ,"priority" .= pack (show (ed ^. priority))
              ,"alert_type" .= pack (show (ed ^. alertType))
              ,"tags" .= (ed ^. tags)
              ]

instance FromJSON EventSpec where
  parseJSON (Object v) = modifyFailure ("EventSpec: " ++) $
                         EventSpec <$>
                         v .: "title" <*>
                         v .: "text" <*>
                         (withScientific "Integer" (\t -> return (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))) =<< v .: "date_happened") <*>
                         v .: "priority" <*>
                         v .:? "host" .!= Nothing <*>
                         v .:? "tags" .!= [] <*>
                         v .:? "alert_type" .!= Info <*>
                         v .:? "source_type" .!= Nothing
  parseJSON a = modifyFailure ("EventSpec: " ++) $ typeMismatch "Object" a

instance ToJSON Event where
  toJSON event = Object $ HM.insert "id" (toJSON (event ^. id')) basemap
    where (Object basemap) = toJSON (event ^. details)

instance FromJSON Event where
  parseJSON (Object v) = modifyFailure ("Event: " ++) $
                         Event <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Event: " ++) $ typeMismatch "Object" a


instance FromJSON WrappedEvent where
  parseJSON (Object v) = modifyFailure ("WrappedEvent: " ++) $
                         WrappedEvent <$> v .: "event"
  parseJSON a = modifyFailure ("WrappedEvent: " ++) $ typeMismatch "Object" a


instance FromJSON WrappedEvents where
  parseJSON (Object v) = modifyFailure ("WrappedEvents: " ++) $
                         WrappedEvents <$> v .: "events"
  parseJSON a = modifyFailure ("WrappedEvents: " ++) $ typeMismatch "Object" a

instance ToJSON Series where
  toJSON s = object [ "series" .= D.toList (fromSeries s) ]

instance ToJSON Timestamp where
  toJSON = toJSON . (round :: NominalDiffTime -> Int) . fromTimestamp

instance ToJSON MetricPoints where
  toJSON (Gauge   ps) = toJSON $ fmap (first Timestamp) ps
  toJSON (Counter ps) = toJSON $ fmap (first Timestamp) ps

instance ToJSON Metric where
  toJSON m = object ks
    where
      f = maybe id (\x y -> ("host" .= x) : y) $ metricHost m
      ks = f [ "metric" .= metricName m
             , "points" .= metricPoints m
             , "tags"   .= metricTags m
             , "type"   .= case metricPoints m of
                 Gauge _   -> "gauge"   :: Text
                 Counter _ -> "counter" :: Text
             ]

instance ToJSON MonitorType where
  toJSON MetricAlert = Data.Aeson.String "metric alert"
  toJSON ServiceCheck = Data.Aeson.String "service check"
  toJSON EventAlert = Data.Aeson.String "event alert"

instance FromJSON MonitorType where
  parseJSON (Data.Aeson.String "metric alert") = return MetricAlert
  -- TODO figure out what "query alert" actually is
  parseJSON (Data.Aeson.String "query alert") = return MetricAlert
  parseJSON (Data.Aeson.String "service check") = return ServiceCheck
  parseJSON (Data.Aeson.String "event alert") = return EventAlert
  parseJSON (Data.Aeson.String s) = fail $ "MonitorType: String " ++ show s ++ " is not a valid MonitorType"
  parseJSON a = modifyFailure ("MonitorType: " ++) $ typeMismatch "String" a

instance ToJSON MonitorOptions where
  toJSON opts = Object $ HM.fromList [ ("silenced", toJSON (opts ^. silenced))
                                     , ("notify_no_data", Bool (opts ^. notifyNoData))
                                     , ("no_data_timeframe", maybe Null (Number . fromIntegral) (opts ^. noDataTimeframe))
                                     , ("timeout_h", maybe Null (Number . fromIntegral) (opts ^. timeoutH))
                                     , ("renotify_interval", maybe Null (Number . fromIntegral) (opts ^. renotifyInterval))
                                     , ("escalation_message", Data.Aeson.String (opts ^. escalationMessage))
                                     , ("notify_audit", Bool (opts ^. notifyAudit))
                                     ]

instance FromJSON MonitorOptions where
  parseJSON (Object v) = modifyFailure ("MonitorOptions: " ++) $
                         MonitorOptions <$>
                         v .:? "silenced" .!= HM.empty <*>
                         v .:? "notify_no_data" .!= False <*>
                         v .:? "no_data_timeframe" .!= Nothing <*>
                         v .:? "timeout_h" .!= Nothing <*>
                         v .:? "renotify_interval" .!= Nothing <*>
                         v .:? "escalation_message" .!= "" <*>
                         v .:? "notify_audit" .!= False
  parseJSON a = modifyFailure ("MonitorOptions: " ++) $ typeMismatch "Object" a

instance ToJSON MonitorSpec where
  toJSON ms = Object $ HM.insert "options" (toJSON (ms ^. options)) hmap
    where (Object hmap) = object $
                          prependMaybe ("name" .=) (ms ^. name) $
                          prependMaybe ("message" .=) (ms ^. message)
                          [ "type" .= pack (show (ms ^. type'))
                          , "query" .= (ms ^. query)
                          ]

-- | Creates the most basic specification required by a monitor, containing the
-- type of monitor and the query string used to detect the monitor's state.
--
-- Generates a set of "default" Monitor options, which specify as little
-- optional configuration as possible.  This includes:
--
--   * No silencing of any part of the monitor
--   * No notification when data related to the monitor is missing
--   * No alert timeout after the monitor is triggeredn
--   * No renotification when the monitor is triggered
--   * No notification when the monitor is modified
--
-- In production situations, it is /not safe/ to rely on this documented
-- default behaviour for critical setitngs; use the helper functions to
-- introspect the MonitorOptions instance provided by this function. This also
-- protects against future modifications to this API.
defaultMonitorOptions :: MonitorOptions
defaultMonitorOptions = MonitorOptions { monitorOptionsSilenced = HM.empty
                                       , monitorOptionsNotifyNoData = False
                                       , monitorOptionsNoDataTimeframe = Nothing
                                       , monitorOptionsTimeoutH = Nothing
                                       , monitorOptionsRenotifyInterval = Nothing
                                       , monitorOptionsEscalationMessage = ""
                                       , monitorOptionsNotifyAudit = False
                                       }

instance FromJSON MonitorSpec where
  parseJSON (Object v) = modifyFailure ("MonitorSpec: " ++) $
                         MonitorSpec <$>
                         v .: "type" <*>
                         v .: "query" <*>
                         v .:? "name" .!= Nothing <*>
                         v .:? "message" .!= Nothing <*>
                         v .:? "options" .!= defaultMonitorOptions
  parseJSON a = modifyFailure ("MonitorSpec: " ++) $ typeMismatch "Object" a

instance ToJSON Monitor where
  toJSON monitor = Object $ HM.insert "id" (toJSON (monitor ^. id')) basemap
    where (Object basemap) = toJSON (monitor ^. spec)

instance FromJSON Monitor where
  parseJSON (Object v) = modifyFailure ("Monitor: " ++ ) $
                         Monitor <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Monitor: " ++) $ typeMismatch "Object" a
