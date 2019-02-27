{-# LANGUAGE OverloadedStrings #-}

{-|
Events in Datadog represent notable occurrences.
-}
module Network.Datadog.Event
( EventPriority(..)
, AlertType(..)
, SourceType(..)
, EventSpec(..)
, Event(eventId', eventDetails)
, EventId
, minimalEventSpec
, createEvent
, loadEvent
, loadEvents
, AsEventPriority(..)
, AsAlertType(..)
, AsSourceType(..)
, HasTitle(..)
, HasText(..)
, HasPriority(..)
, HasDateHappened(..)
, HasAlertType(..)
, HasDetails(..)
, HasTags(..)
, HasSourceType(..)
, HasHost(..)
, HasId'(..)
) where


import Control.Monad (liftM)

import Data.Aeson hiding (Error, Success)
-- import qualified Data.Aeson (Result(Success))
import Data.List (intercalate)
import Data.Text (Text, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.HTTP.Types

import Network.Datadog.Internal

-- | Creates the most basic description required for an event, containing the
-- event title, descriptive text, time of occurrence, and priority of the
-- event. This event will be of type Info.
minimalEventSpec :: Text -> Text -> UTCTime -> EventPriority -> EventSpec
minimalEventSpec specTitle specText time eventPriority = EventSpec
  { eventSpecTitle = specTitle
  , eventSpecText = specText
  , eventSpecDateHappened = time
  , eventSpecPriority = eventPriority
  , eventSpecHost = Nothing
  , eventSpecTags = []
  , eventSpecAlertType = Info
  , eventSpecSourceType = Nothing
  }


-- | Store a new event in Datadog.
createEvent :: Environment -> EventSpec -> IO Event
createEvent env eventSpec =
  let path = "events"
  in liftM wrappedEvent $
     datadogHttp env path [] POST (Just $ encode eventSpec) >>=
     decodeDatadog "createEvent"


-- | Load an event from Datadog by its ID.
loadEvent :: Environment -> EventId -> IO Event
loadEvent env eventId =
  let path = "events/" ++ show eventId
  in liftM wrappedEvent $
     datadogHttp env path [] GET Nothing >>=
     decodeDatadog "loadEvent"

-- | Query Datadog for events within a specific time range.
loadEvents :: Environment
           -> (UTCTime,UTCTime)
           -- ^ The range within which to query for events
           -> Maybe EventPriority
           -- ^ Optionally filter results by a specific priority level
           -> [Text]
           -- ^ A list of tags to filter by
           -> IO [Event]
loadEvents env (startTime, endTime) resultPriority filterTags =
  let path = "events"
      q = prependMaybe (\a -> ("priority", show a)) resultPriority $
            prependBool (not $ null filterTags) ("tags", intercalate "," (map unpack filterTags))
            [("start", show (floor (utcTimeToPOSIXSeconds startTime) :: Integer))
            ,("end", show (floor (utcTimeToPOSIXSeconds endTime) :: Integer))
            ]
  in liftM wrappedEvents $
     datadogHttp env path q GET Nothing >>=
     decodeDatadog "loadEvent"

