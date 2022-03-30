{-| DogStatsD accepts custom application metrics points over UDP, and then periodically aggregates and forwards the metrics to Datadog, where they can be graphed on dashboards. The data is sent by using a client library such as this one that communicates with a DogStatsD server. -}

{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
module Network.StatsD.Datadog (
  -- * Client interface
  DogStatsSettings(..),
  defaultSettings,
  withDogStatsD,
  mkStatsClient,
  finalizeStatsClient,
  send,
  -- * Data supported by DogStatsD
  metric,
  Metric,
  MetricName(..),
  MetricType(..),
  event,
  Event,
  serviceCheck,
  ServiceCheck,
  ServiceCheckStatus(..),
  ToStatsD,
  -- * Optional fields
  Tag(fromTag),
  tag,
  ToMetricValue(..),
  value,
  Priority(..),
  AlertType(..),
  HasName(..),
  HasSampleRate(..),
  HasType'(..),
  HasTags(..),
  HasTitle(..),
  HasText(..),
  HasDateHappened(..),
  HasHostname(..),
  HasAggregationKey(..),
  HasPriority(..),
  HasSourceTypeName(..),
  HasAlertType(..),
  HasHost(..),
  HasPort(..),
  HasBufferSize(..),
  HasMaxDelay(..),
  HasOnException(..),
  HasStatus(..),
  HasMessage(..),
  -- * Dummy client
  StatsClient(Dummy),
  MetricLargerThanBufferSizeException
) where
import Control.Applicative ((<$>))
import Control.Exception (SomeException)
import Control.Lens
import Control.Monad (void, when)
import Control.Reaper
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.BufferBuilder.Utf8
import Data.Function (on)
import Data.List (intersperse)
import qualified Data.Sequence as Seq
import qualified Data.ByteString as B
import qualified Data.Foldable as F
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock
import Data.Time.Clock.POSIX
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.Socket hiding (send, sendTo, recv, recvFrom)
import System.IO
  ( BufferMode(BlockBuffering)
  , Handle
  , IOMode(WriteMode)
  )
import UnliftIO

epochTime :: UTCTime -> Int
epochTime = round . utcTimeToPOSIXSeconds

newtype MetricName = MetricName { fromMetricName :: Text }

cleanMetricText :: Text -> Text
cleanMetricText = T.map $ \c -> case c of
  ':' -> '_'
  '|' -> '_'
  '@' -> '_'
  _   -> c
{-# INLINE cleanMetricText #-}

escapeEventContents :: T.Text -> T.Text
escapeEventContents = T.replace "\n" "\\n"
{-# INLINE escapeEventContents #-}

-- | Tags are a Datadog specific extension to StatsD. They allow you to tag a metric with a
-- dimension that’s meaningful to you and slice and dice along that dimension in your graphs.
-- For example, if you wanted to measure the performance of two video rendering algorithms,
-- you could tag the rendering time metric with the version of the algorithm you used.
newtype Tag = Tag { fromTag :: Utf8Builder () }

instance Show Tag where show = ("Tag " ++) . T.unpack . decodeUtf8 . runUtf8Builder . fromTag
instance Eq Tag where (==) = (==) `on` show

-- | Create a tag from a key-value pair. Useful for slicing and dicing events in Datadog.
--
-- Key and value text values are normalized by converting ":"s, "|"s, and "@"s to underscores ("_").
tag :: Text -> Text -> Tag
tag k v = Tag (build k >> appendChar7 ':' >> build v)
  where
    build = appendText . cleanMetricText

data MetricType = Gauge      -- ^ Gauges measure the value of a particular thing at a particular time, like the amount of fuel in a car’s gas tank or the number of users connected to a system.
                | Counter    -- ^ Counters track how many times something happened per second, like the number of database requests or page views.
                | Timer      -- ^ StatsD only supports histograms for timing, not generic values (like the size of uploaded files or the number of rows returned from a query). Timers are essentially a special case of histograms, so they are treated in the same manner by DogStatsD for backwards compatibility.
                | Histogram  -- ^ Histograms track the statistical distribution of a set of values, like the duration of a number of database queries or the size of files uploaded by users. Each histogram will track the average, the minimum, the maximum, the median and the 95th percentile.
                | Set        -- ^ Sets are used to count the number of unique elements in a group. If you want to track the number of unique visitor to your site, sets are a great way to do that.

-- | Converts a supported numeric type to the format understood by DogStatsD. Currently limited by BufferBuilder encoding options.
class ToMetricValue a where
  encodeValue :: a -> Utf8Builder ()

instance ToMetricValue Int where
  encodeValue = appendDecimalSignedInt

instance ToMetricValue Double where
  encodeValue = appendDecimalDouble

-- | Smart 'Metric' constructor. Use the lens functions to set the optional fields.
metric :: (ToMetricValue a) => MetricName -> MetricType -> a -> Metric
metric n t v = Metric n 1 t (encodeValue v) []

-- | 'Metric'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'name' @::@ 'MetricName'
--
-- * 'sampleRate' @::@ 'Double'
--
-- * 'type'' @::@ 'MetricType'
--
-- * 'value' @::@ 'ToMetricValue' @a => a@
--
-- * 'tags' @::@ @[@'Tag'@]@
data Metric = Metric
  { metricName       :: !MetricName
  , metricSampleRate :: {-# UNPACK #-} !Double
  , metricType'      :: !MetricType
  , mValue           :: !(Utf8Builder ())
  , metricTags       :: ![Tag]
  }

makeFields ''Metric

-- | Special setter to update the value of a 'Metric'.
--
-- > metric ("foo"" :: Text) Counter (1 :: Int) & value .~ (5 :: Double)
value :: ToMetricValue a => Setter Metric Metric (Utf8Builder ()) a
value = sets $ \f m -> m { mValue = encodeValue $ f $ mValue m }
{-# INLINE value #-}

renderMetric :: Metric -> Utf8Builder ()
renderMetric (Metric n sr t v ts) = do
  appendText $ cleanMetricText $ fromMetricName n
  appendChar7 ':'
  v
  appendChar7 '|'
  unit
  formatRate
  formatTags
  where
    unit = case t of
      Gauge     -> appendChar7 'g'
      Counter   -> appendChar7 'c'
      Timer     -> appendBS7 "ms"
      Histogram -> appendChar7 'h'
      Set       -> appendChar7 's'
    formatTags = case ts of
      [] -> return ()
      xs -> appendBS7 "|#" >> F.sequence_ (intersperse (appendChar7 ',') $ map fromTag xs)
    formatRate = if sr == 1 then return () else appendBS7 "|@" >> appendDecimalDouble sr

data Priority = Low | Normal
data AlertType = Error | Warning | Info | Success

-- | Smart 'Event' constructor. Use the lens functions to set the optional fields.
event :: Text -> Text -> Event
event t d = Event t d Nothing Nothing Nothing Nothing Nothing Nothing []

-- | 'Event'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'title' @::@ 'Text'
--
-- * 'text' @::@ 'Text'
--
-- * 'dateHappened' @::@ 'Maybe' 'UTCTime'
--
-- * 'hostname' @::@ 'Maybe' 'Text'
--
-- * 'aggregationKey' @::@ 'Maybe' 'Text'
--
-- * 'priority' @::@ 'Maybe' 'Priority'
--
-- * 'sourceTypeName' @::@ 'Maybe' 'Text'
--
-- * 'alertType' @::@ 'Maybe' 'AlertType'
--
-- * 'tags' @::@ @[@'Tag'@]@
--
data Event = Event
  { eventTitle          :: {-# UNPACK #-} !Text
  , eventText           :: {-# UNPACK #-} !Text
  , eventDateHappened   :: !(Maybe UTCTime)
  , eventHostname       :: !(Maybe Text)
  , eventAggregationKey :: !(Maybe Text)
  , eventPriority       :: !(Maybe Priority)
  , eventSourceTypeName :: !(Maybe Text)
  , eventAlertType      :: !(Maybe AlertType)
  , eventTags           :: ![Tag]
  }

makeFields ''Event

renderEvent :: Event -> Utf8Builder ()
renderEvent e = do
  appendBS7 "_e{"
  encodeValue $ B.length escapedTitle
  appendChar7 ','
  encodeValue $ B.length escapedText
  appendBS7 "}:"
  -- This is safe because we encodeUtf8 below
  -- We do so to get the length of the ultimately encoded bytes for the datagram format
  unsafeAppendBS escapedTitle
  appendChar7 '|'
  -- This is safe because we encodeUtf8 below
  -- We do so to get the length of the ultimately encoded bytes for the datagram format
  unsafeAppendBS escapedText
  happened
  formatHostname
  aggregation
  formatPriority
  sourceType
  alert
  formatTags
  where
    escapedTitle = encodeUtf8 $ escapeEventContents $ eventTitle e
    escapedText = encodeUtf8 $ escapeEventContents $ eventText e
    makeField c v = F.forM_ v $ \jv ->
      appendChar7 '|' >> appendChar7 c >> appendChar7 ':' >> jv
    cleanTextValue f = (appendText . cleanMetricText) <$> f e
    -- TODO figure out the actual format that dateHappened values are supposed to have.
    happened = F.forM_ (eventDateHappened e) $ \h -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime h
    formatHostname = makeField 'h' $ cleanTextValue eventHostname
    aggregation = makeField 'k' $ cleanTextValue eventAggregationKey
    formatPriority = F.forM_ (eventPriority e) $ \p -> do
      appendBS7 "|p:"
      appendBS7 $ case p of
        Low    -> "low"
        Normal -> "normal"
    sourceType = makeField 's' $ cleanTextValue eventSourceTypeName
    alert = F.forM_ (eventAlertType e) $ \a -> do
              appendBS7 "|t:"
              appendBS7 $ case a of
                Error   -> "error"
                Warning -> "warning"
                Info    -> "info"
                Success -> "success"
    formatTags = case eventTags e of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map fromTag ts

data ServiceCheckStatus
  = ServiceOk
  | ServiceWarning
  | ServiceCritical
  | ServiceUnknown
  deriving (Read, Show, Eq, Ord, Enum)

-- | 'ServiceCheck'
--
-- The fields accessible through corresponding lenses are:
--
-- * 'name' @::@ 'Text'
--
-- * 'status' @::@ 'ServiceCheckStatus'
--
-- * 'message' @::@ 'Maybe' 'Text'
--
-- * 'dateHappened' @::@ 'Maybe' 'UTCTime'
--
-- * 'hostname' @::@ 'Maybe' 'Text'
--
-- * 'tags' @::@ @[@'Tag'@]@
data ServiceCheck = ServiceCheck
  { serviceCheckName         :: {-# UNPACK #-} !Text
  , serviceCheckStatus       :: !ServiceCheckStatus
  , serviceCheckMessage      :: !(Maybe Text)
  , serviceCheckDateHappened :: !(Maybe UTCTime)
  , serviceCheckHostname     :: !(Maybe Text)
  , serviceCheckTags         :: ![Tag]
  }

makeFields ''ServiceCheck

serviceCheck ::
     Text -- ^ name
  -> ServiceCheckStatus
  -> ServiceCheck
serviceCheck n s = ServiceCheck n s Nothing Nothing Nothing []

-- | Convert an 'Event', 'Metric', or 'StatusCheck' to their wire format.
class ToStatsD a where
  toStatsD :: a -> Utf8Builder ()

instance ToStatsD Metric where
  toStatsD = renderMetric

instance ToStatsD Event where
  toStatsD = renderEvent

instance ToStatsD ServiceCheck where
  toStatsD check = do
    appendBS7 "_sc|"
    appendText $ cleanMetricText $ check ^. name
    appendChar7 '|'
    appendDecimalSignedInt $ fromEnum $ check ^. status
    F.forM_ (check ^. message) $ \msg ->
      appendBS7 "|m:" >> appendText (cleanMetricText msg)
    F.forM_ (check ^. dateHappened) $ \ts -> do
      appendBS7 "|d:"
      appendDecimalSignedInt $ epochTime ts
    F.forM_ (check ^. hostname) $ \hn ->
      appendBS7 "|h:" >> appendText (cleanMetricText hn)
    case check ^. tags of
      [] -> return ()
      ts -> do
        appendBS7 "|#"
        sequence_ $ intersperse (appendChar7 ',') $ map fromTag ts

data DogStatsSettings = DogStatsSettings
  { dogStatsSettingsHost        :: HostName -- ^ The hostname or IP of the DogStatsD server (default: 127.0.0.1)
  , dogStatsSettingsPort        :: !Int -- ^ The port that the DogStatsD server is listening on (default: 8125)
  , dogStatsSettingsBufferSize  :: !Int -- ^ Maximum buffer size. Stats are sent over UDP, so the maximum possible value is 65507 bytes per packet. In some scenarios, however, you may wish to send smaller packets. (default: 65507)
  , dogStatsSettingsMaxDelay    :: !Int -- ^ Maximum amount of time (in microseconds) between having no stats to send locally and when new stats will be sent to the statsd server. (default: 1 second)
  , dogStatsSettingsOnException :: (SomeException -> Seq.Seq ByteString -> IO (Seq.Seq ByteString -> Seq.Seq ByteString)) -- ^ Handler to recover from exceptions thrown while sending stats to the server. Caution: Throwing an exception from this handler will shut down the worker that sends stats to the server, but is not able to prevent you from enqueuing stats via the client. Default: print the exception and throw away any accumulated stats.
  }

makeFields ''DogStatsSettings

defaultSettings :: DogStatsSettings
defaultSettings =
  DogStatsSettings
    { dogStatsSettingsHost = "127.0.0.1"
    , dogStatsSettingsPort = 8125
    , dogStatsSettingsBufferSize = 65507
    , dogStatsSettingsMaxDelay = 1000000
    , dogStatsSettingsOnException =
        \e _ ->
          putStrLn
            (show e ++
             "\nDropping all accumulated stats due to error. This behavior may be overridden by setting the onException handler of DogStatsSettings.") >>
          return (const Seq.empty)
    }

accumulateStats ::
     Int -- ^ Max buffer size
  -> Seq.Seq ByteString -- ^ Items to send
  -> (L.ByteString, Seq.Seq ByteString)
accumulateStats maxBufSize = go 0 []
  where
    go :: Int -> [ByteString] -> Seq.Seq ByteString -> (L.ByteString, Seq.Seq ByteString)
    go !accum chunks s = case Seq.viewl s of
      Seq.EmptyL -> (finalizeChunks chunks, Seq.empty)
      (bs Seq.:< rest) ->
        let newChunkSize = B.length bs
            newTotalSize = newChunkSize + accum
         in if newChunkSize > maxBufSize
            then error "Oversized chunk made it into datadog accumulateStats. Please report this as a bug."
            else if newTotalSize > maxBufSize
                 then (finalizeChunks chunks, s)
                 else go newTotalSize (bs : chunks) rest

    finalizeChunks :: [ByteString] -> L.ByteString
    finalizeChunks = L.fromChunks . reverse

-- | Create a stats client. Be sure to close it with 'finalizeStatsClient' in order to send any pending stats and close the underlying handle when done using it. Alternatively, use 'withDogStatsD' to finalize it automatically.
mkStatsClient :: MonadIO m => DogStatsSettings -> m StatsClient
mkStatsClient s = liftIO $ do
  addrInfos <- getAddrInfo
               (Just $ defaultHints { addrFlags = [AI_PASSIVE] })
               (Just $ s ^. host)
               (Just $ show $ s ^. port)
  case addrInfos of
    [] -> error "No address for hostname" -- TODO throw
    (serverAddr:_) -> do
      sock <- socket (addrFamily serverAddr) Datagram defaultProtocol
      connect sock (addrAddress serverAddr)
      h <- socketToHandle sock WriteMode
      hSetBuffering h (BlockBuffering $ Just $ dogStatsSettingsBufferSize s)
      let reaperSettings = defaultReaperSettings
            { reaperAction = \stats -> catch (builderAction h (dogStatsSettingsBufferSize s) stats) $ \e ->
                dogStatsSettingsOnException s e stats
            , reaperDelay = dogStatsSettingsMaxDelay s
            , reaperCons = \item work -> work Seq.|> item
            , reaperNull = Seq.null
            , reaperEmpty = Seq.empty
            }
      r <- mkReaper reaperSettings
      return $ StatsClient h r s

builderAction :: Handle -> Int -> Seq.Seq ByteString -> IO (Seq.Seq ByteString -> Seq.Seq ByteString)
builderAction h maxBufSize s = case Seq.viewl s of
  Seq.EmptyL -> return $ const Seq.empty
  _ -> do
    let (toFlush, rest) = accumulateStats maxBufSize s
    L.hPut h toFlush
    hFlush h -- safety flush
    builderAction h maxBufSize rest

-- | Create a 'StatsClient' and provide it to the provided function. The 'StatsClient' will be finalized as soon as the inner block is exited, whether normally or via an exception.
withDogStatsD :: MonadUnliftIO m => DogStatsSettings -> (StatsClient -> m a) -> m a
withDogStatsD s = bracket (mkStatsClient s) finalizeStatsClient

-- | Note that Dummy is not the only constructor, just the only publicly available one.
data StatsClient = StatsClient
                   { statsClientHandle :: !Handle
                   , statsClientReaper :: Reaper (Seq.Seq ByteString) ByteString
                   , statsClientSettings :: DogStatsSettings
                   }
                 | Dummy -- ^ Just drops all stats.

-- | Send a 'Metric', 'Event', or 'StatusCheck' to the DogStatsD server.
--
-- Since UDP is used to send the events,
-- there is no ack that sent values are successfully dealt with.
--
-- > withDogStatsD defaultSettings $ \client -> do
-- >   send client $ event "Wombat attack" "A host of mighty wombats has breached the gates"
-- >   send client $ metric "wombat.force_count" Gauge (9001 :: Int)
-- >   send client $ serviceCheck "Wombat Radar" ServiceOk
send :: (MonadIO m, ToStatsD v) => StatsClient -> v -> m ()
send StatsClient {statsClientReaper, statsClientSettings} v = do
  let bytes = runUtf8Builder (toStatsD v >> appendChar7 '\n')
      bytesSize = B.length bytes
      maxBufSize = dogStatsSettingsBufferSize statsClientSettings

  when (bytesSize > maxBufSize) $ throwIO $
    MetricLargerThanBufferSizeException
      { metricSize = bytesSize
      , maxBufferSize = maxBufSize
      }

  liftIO $ reaperAdd statsClientReaper bytes
send Dummy _ = return ()
{-# INLINEABLE send #-}

-- | Send all pending unsent events and close the connection to the specified statsd server.
finalizeStatsClient :: MonadIO m => StatsClient -> m ()
finalizeStatsClient (StatsClient h r s) = liftIO $ do
  remainingStats <- reaperStop r
  void $ builderAction h (dogStatsSettingsBufferSize s) remainingStats
  hClose h
finalizeStatsClient Dummy = return ()

data MetricLargerThanBufferSizeException =
  MetricLargerThanBufferSizeException
    { metricSize :: Int
    , maxBufferSize :: Int
    } deriving (Show, Typeable)

instance Exception MetricLargerThanBufferSizeException
