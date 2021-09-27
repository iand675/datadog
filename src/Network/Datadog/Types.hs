{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE TemplateHaskell            #-}
module Network.Datadog.Types where
import           Data.ByteString.Char8 (ByteString)
import           Data.DList (DList)
import           Data.HashMap.Strict (HashMap)
import           Data.Semigroup
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Int (Int64)
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Time.Clock.POSIX (POSIXTime)
import           Network.HTTP.Client (Manager)

newtype Timestamp = Timestamp { fromTimestamp :: NominalDiffTime }

newtype Write = Write { writeApiKey :: ByteString }

data ReadWrite = ReadWrite { readWriteApiKey :: ByteString
                           , readWriteApplicationKey :: ByteString
                           }

data DatadogClient a = DatadogClient
  { datadogClientManager        :: Manager
  , datadogClientKeys           :: a
  }


-- | Wraps the keys needed by Datadog to fully access the API.
data Keys = Keys { apiKey :: String
                   -- A write-key associated with a user
                 , appKey :: String
                   -- A read-key associated with an application
                 } deriving (Eq)


-- | An Environment contains everything needed to interact with Datadog.
data Environment = Environment { environmentKeys :: Keys
                                 -- ^ Auth keys to permit communication with Datadog
                               , environmentApiUrl :: String
                                 -- ^ The root URL for the Datadog API
                               , environmentManager :: Manager
                                 -- ^ HTTP manager used to make requests to Datadog
                               }

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
data Tag = KeyValueTag Text Text
         | LabelTag Text
         deriving (Eq)

instance Show Tag where
  show (KeyValueTag k v) = T.unpack k ++ (':' : T.unpack v)
  show (LabelTag t) = T.unpack t

instance Read Tag where
  readsPrec _ s = let t = T.pack s
                  in (\a -> [(a, "")]) $
                     maybe (LabelTag t) (\i -> uncurry KeyValueTag (T.splitAt i t)) $
                     T.findIndex (==':') t

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

-- | The result of running a check on some service.
data CheckResult = CheckResult { checkResultCheck :: Text
                                 -- ^ Text describing the check
                               , checkResultHostName :: Text
                                 -- ^ Name of the host which the check applies to
                               , checkResultStatus :: CheckStatus
                                 -- ^ Status result of the check
                               , checkResultTimestamp :: Maybe UTCTime
                                 -- ^ Time at which the check occurred (Nothing will wait until the
                                 -- check is sent to Datadog to compute the time)
                               , checkResultMessage :: Maybe Text
                                 -- ^ Information related to why this specific check run supplied
                                 -- the status it did
                               , checkResultTags :: [Tag]
                                 -- ^ Tags to associate with this check run
                               } deriving (Eq)

-- | A description of when downtime should occur.
data DowntimeSpec = DowntimeSpec { downtimeSpecStart :: Maybe UTCTime
                                   -- ^ When to start the downtime (or immediately)
                                 , downtimeSpecEnd :: Maybe UTCTime
                                   -- ^ When to stop the downtime (or indefinitely)
                                 , downtimeSpecMessage :: Maybe Text
                                   -- ^ A message to include with notifications for this downtime
                                 , downtimeSpecScope :: Tag
                                   -- ^ The scope to apply downtime to (if applying downtime to a
                                   -- host, use a tag of the form "host:hostname", NOT just
                                   -- "hostname")
                                 } deriving (Eq)


-- | Datadog's internal reference to a specific donwtime instance.
type DowntimeId = Int


-- | A scheduled donwtime stored in Datadog.
data Downtime = Downtime { downtimeId' :: DowntimeId
                           -- ^ Datadog's unique reference to the scheduled downtime
                         , downtimeSpec :: DowntimeSpec
                           -- ^ Context on the downtime schedule
                         } deriving (Eq)


-- | A set of priorities used to denote the importance of an event.
data EventPriority = NormalPriority
                   | LowPriority
                   deriving (Eq)

instance Show EventPriority where
  show NormalPriority = "normal"
  show LowPriority = "low"

-- | The failure levels for an alert.
data AlertType = Error
               | Warning
               | Info
               | Success
               deriving (Eq)

instance Show AlertType where
  show Error = "error"
  show Warning = "warning"
  show Info = "info"
  show Success = "success"

-- | A source from which an event may originate, recognized by Datadog.
data SourceType = Nagios
                | Hudson
                | Jenkins
                | User
                | MyApps
                | Feed
                | Chef
                | Puppet
                | Git
                | BitBucket
                | Fabric
                | Capistrano
                deriving (Eq)

instance Show SourceType where
  show Nagios = "nagios"
  show Hudson = "hudson"
  show Jenkins = "jenkins"
  show User = "user"
  show MyApps = "my apps"
  show Feed = "feed"
  show Chef = "chef"
  show Puppet = "puppet"
  show Git = "git"
  show BitBucket = "bitbucket"
  show Fabric = "fabric"
  show Capistrano = "capistrano"

-- | Details that describe an event.
data EventSpec = EventSpec { eventSpecTitle :: Text
                           , eventSpecText :: Text
                             -- ^ The description/body of the event
                           , eventSpecDateHappened :: UTCTime
                             -- ^ The time at which the event occurred
                           , eventSpecPriority :: EventPriority
                           , eventSpecHost :: Maybe Text
                             -- ^ The hostname associated with the event
                           , eventSpecTags :: [Tag]
                           , eventSpecAlertType :: AlertType
                           , eventSpecSourceType :: Maybe SourceType
                             -- ^ The trigger of the event (if identifiable)
                           } deriving (Eq, Show)

-- | Datadog's internal reference to a specific event.
type EventId = Int

-- | An event stored within Datadog. An event represents some sort of
-- occurrence that was recorded in Datadog.
data Event = Event { eventId' :: EventId
                     -- ^ Datadog's unique reference to the event
                   , eventDetails :: EventSpec
                     -- ^ Context on what happened during this event
                   } deriving (Eq, Show)

data WrappedEvent = WrappedEvent { wrappedEvent :: Event }

data WrappedEvents = WrappedEvents { wrappedEvents :: [Event] }

newtype Series = Series { fromSeries :: DList Metric }
                 deriving (Semigroup, Monoid)

data MetricPoints = Gauge   [(POSIXTime, Float)]
                  | Counter [(POSIXTime, Int64)]

data Metric = Metric
  { metricName   :: Text
  , metricPoints :: MetricPoints
  , metricHost   :: Maybe Text
  , metricTags   :: [Text]
  , metricInterval :: Maybe Integer
  }

-- | Each monitor is of a specific type, which determines what sort of check
-- the monitor performs.
data MonitorType = MetricAlert
                   -- ^ Watches a (combination of) metric(s), alerting when it
                   -- crosses some threshold.
                 | ServiceCheck
                   -- ^ Watches a service and alerts when the service enters a
                   -- failing state.
                 | EventAlert
                   -- ^ Checks the event stream for events meeting certain
                   -- criteria.
                 deriving (Eq)

instance Show MonitorType where
  show MetricAlert = "metric alert"
  show ServiceCheck = "service check"
  show EventAlert = "event alert"

-- | Advanced configuration parameters for a monitor.
data MonitorOptions = MonitorOptions { monitorOptionsSilenced :: HashMap T.Text (Maybe Integer)
                                     , monitorOptionsNotifyNoData :: Bool
                                     , monitorOptionsNoDataTimeframe :: Maybe Integer
                                     , monitorOptionsTimeoutH :: Maybe Integer
                                     , monitorOptionsRenotifyInterval :: Maybe Integer
                                     , monitorOptionsEscalationMessage :: T.Text
                                     , monitorOptionsNotifyAudit :: Bool
                                     } deriving (Eq)

-- | A representation of a monitor's configuration, from which a monitor could
-- be rebuilt.
data MonitorSpec = MonitorSpec { monitorSpecType' :: MonitorType
                               , monitorSpecQuery :: T.Text
                                 -- ^ The query string the monitor uses to
                                 -- determine its state.
                               , monitorSpecName :: Maybe T.Text
                                 -- ^ The human-readable name of the monitor.
                               , monitorSpecMessage :: Maybe T.Text
                                 -- ^ The message sent with the notification
                                 -- when the monitor is triggered.
                               , monitorSpecOptions :: MonitorOptions
                                 -- ^ Optional configuration parameters
                                 -- specifying advanced monitor beahviour.
                               } deriving (Eq)

-- | Datadog's internal reference to a specific monitor.
type MonitorId = Int

-- | A Datadog monitor. These monitors actively check multiple different types
-- of data within Datadog against user-provided conditions, triggering
-- notifications when condition(s) are met.
data Monitor = Monitor { monitorId' :: MonitorId
                         -- ^ Datadog's internal reference to this specific
                         -- monitor.
                       , monitorSpec :: MonitorSpec
                         -- ^ The specification from which this monitor can be
                         -- re-created.
                       } deriving (Eq)
