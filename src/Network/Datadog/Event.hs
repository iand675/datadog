{-# LANGUAGE OverloadedStrings #-}

{-|
Events in Datadog represent notable occurrences.
-}
module Network.Datadog.Event
( EventPriority(..)
, AlertType(..)
, SourceType(..)
, EventSpec(..)
, Event
, eId
, eDetails
, EventId
, minimalEventSpec
, createEvent
, loadEvent
, loadEvents
) where


import Data.Aeson hiding (Error, Success)
import Data.Aeson.Types (modifyFailure, typeMismatch)
-- import qualified Data.Aeson (Result(Success))
import qualified Data.HashMap.Strict as Data.HashMap
import Data.List (intercalate)
import Data.HashSet (HashSet)
import qualified Data.HashSet as HashSet
import Data.Text (Text, pack, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Vector (toList)

import Network.HTTP.Conduit

import Network.Datadog
import Network.Datadog.Internal


-- | A set of priorities used to denote the importance of an event.
data EventPriority = NormalPriority
                   | LowPriority
                   deriving (Eq)

instance Show EventPriority where
  show NormalPriority = "normal"
  show LowPriority = "low"

instance ToJSON EventPriority where
  toJSON NormalPriority = Data.Aeson.String "normal"
  toJSON LowPriority = Data.Aeson.String "low"

instance FromJSON EventPriority where
  parseJSON (Data.Aeson.String "normal") = return NormalPriority
  parseJSON (Data.Aeson.String "low") = return LowPriority
  parseJSON (Data.Aeson.String s) = fail $ "EventPriority: String \"" ++ unpack s ++ "\" is not a valid EventPriority"
  parseJSON a = modifyFailure ("EventPriority: " ++) $ typeMismatch "String" a


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
  parseJSON (Data.Aeson.String s) = fail $ "AlertType: String \"" ++ unpack s ++ "\" is not a valid AlertType"
  parseJSON a = modifyFailure ("AlertType: " ++) $ typeMismatch "String" a


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
  parseJSON (Data.Aeson.String s) = fail $ "SourceType: String \"" ++ unpack s ++ "\" is not a valid SourceType"
  parseJSON a = modifyFailure ("SourceType: " ++) $ typeMismatch "String" a


-- | Details that describe an event.
data EventSpec = EventSpec { edTitle :: Text
                           , edText :: Text
                             -- ^ The description/body of the event
                           , edDateHappened :: UTCTime
                             -- ^ The time at which the event occurred
                           , edPriority :: EventPriority
                           , edHost :: Maybe Text
                             -- ^ The hostname associated with the event
                           , edTags :: HashSet Text
                           , edAlertType :: AlertType
                           , edSourceType :: Maybe SourceType
                             -- ^ The trigger of the event (if identifiable)
                           } deriving (Eq, Show)

instance ToJSON EventSpec where
  toJSON ed = object (["title" .= edTitle ed
                      ,"text" .= edText ed
                      ,"date_happened" .= (floor (utcTimeToPOSIXSeconds (edDateHappened ed)) :: Integer)
                      ,"priority" .= pack (show (edPriority ed))
                      ,"alert_type" .= pack (show (edAlertType ed))
                      ,"tags" .= HashSet.toList (edTags ed)]
                      ++ maybe [] (\a -> ["host" .= a]) (edHost ed)
                      ++ maybe [] (\a -> ["source_type_name" .= pack (show a)]) (edSourceType ed)
                     )

instance FromJSON EventSpec where
  parseJSON (Object v) = modifyFailure ("EventSpec: " ++) $
                         EventSpec <$>
                         v .: "title" <*>
                         v .: "text" <*>
                         (withScientific "Integer" (\t -> return (posixSecondsToUTCTime (fromIntegral (floor t :: Integer)))) =<< v .: "date_happened") <*>
                         v .: "priority" <*>
                         v .:? "host" .!= Nothing <*>
                         (withArray "List" (fmap HashSet.fromList . mapM (withText "Text" return) . toList) =<< v .: "tags") <*>
                         v .:? "alert_type" .!= Info <*>
                         v .:? "source_type" .!= Nothing
  parseJSON a = modifyFailure ("EventSpec: " ++) $ typeMismatch "Object" a


-- | Creates the most basic description required for an event, containing the
-- event title, descriptive text, time of occurrence, and priority of the
-- event. This event will be of type Info.
minimalEventSpec :: Text -> Text -> UTCTime -> EventPriority -> EventSpec
minimalEventSpec title text time eventPriority = EventSpec { edTitle = title
                                                           , edText = text
                                                           , edDateHappened = time
                                                           , edPriority = eventPriority
                                                           , edHost = Nothing
                                                           , edTags = HashSet.empty
                                                           , edAlertType = Info
                                                           , edSourceType = Nothing
                                                           }


-- | Datadog's internal reference to a specific event.
type EventId = Int

-- | An event stored within Datadog. An event represents some sort of
-- occurrence that was recorded in Datadog.
data Event = Event { eId :: EventId
                     -- ^ Datadog's unique reference to the event
                   , eDetails :: EventSpec
                     -- ^ Context on what happened during this event
                   } deriving (Eq, Show)

instance ToJSON Event where
  toJSON event = Object $ Data.HashMap.union basemap newmap
    where (Object basemap) = toJSON (eDetails event)
          (Object newmap) = object ["id" .= eId event]

instance FromJSON Event where
  parseJSON (Object v) = modifyFailure ("Event: " ++) $
                         Event <$> v .: "id" <*> parseJSON (Object v)
  parseJSON a = modifyFailure ("Event: " ++) $ typeMismatch "Object" a


data WrappedEvent = WrappedEvent { wrappedEvent :: Event }

instance FromJSON WrappedEvent where
  parseJSON (Object v) = modifyFailure ("WrappedEvent: " ++) $
                         WrappedEvent <$> v .: "event"
  parseJSON a = modifyFailure ("WrappedEvent: " ++) $ typeMismatch "Object" a

data WrappedEvents = WrappedEvents { wrappedEvents :: [Event] }

instance FromJSON WrappedEvents where
  parseJSON (Object v) = modifyFailure ("WrappedEvents: " ++) $
                         WrappedEvents <$> v .: "events"
  parseJSON a = modifyFailure ("WrappedEvents: " ++) $ typeMismatch "Object" a


-- | Store a new event in Datadog.
createEvent :: Environment -> EventSpec -> IO Event
createEvent (Environment keys manager) eventDetails = do
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/events?api_key=" ++ apiKey keys
  let request = initReq { method = "POST"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode eventDetails)
                        }
  resp <- httpLbs request manager
  fmap wrappedEvent $ decodeDatadog "createEvent" $ responseBody resp


-- | Load an event from Datadog by its ID.
loadEvent :: Environment -> EventId -> IO Event
loadEvent (Environment keys manager) eventId = do
  request <- parseUrl $ "https://app.datadoghq.com/api/v1/events/" ++ show eventId
             ++ "?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
  resp <- httpLbs request manager
  fmap wrappedEvent $ decodeDatadog "loadEvent" $ responseBody resp


-- | Query Datadog for events within a specific time range.
loadEvents :: Environment
           -> (UTCTime,UTCTime)
           -- ^ The range within which to query for events
           -> Maybe EventPriority
           -- ^ Optionally filter results by a specific priority level
           -> [Text]
           -- ^ A list of tags to filter by
           -> IO [Event]
loadEvents (Environment keys manager) (start,end) priority tags = do
  request <- parseUrl $ "https://app.datadoghq.com/api/v1/events?api_key=" ++ apiKey keys
             ++ "&application_key=" ++ appKey keys
             ++ "&start=" ++ show (floor (utcTimeToPOSIXSeconds start) :: Integer)
             ++ "&end=" ++ show (floor (utcTimeToPOSIXSeconds end) :: Integer)
             ++ maybe "" (\a -> "&priority=" ++ show a) priority
             ++ if null tags then "" else "&tags=" ++ intercalate "," (map unpack tags)
  resp <- httpLbs request manager
  fmap wrappedEvents $ decodeDatadog "loadEvents" $ responseBody resp
