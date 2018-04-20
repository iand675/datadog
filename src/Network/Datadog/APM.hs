{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Datadog.APM where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import Data.Atomics.Counter
import qualified Data.HashMap.Strict as H
import Data.IORef.Lifted
import Data.String
import Data.Text (Text, pack)
import Data.Word
import GHC.Stack
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import System.Clock

newtype AppName = AppName Text
  deriving (Show, Eq, Ord, IsString)

instance ToJSON AppName where
  toJSON (AppName t) = toJSON t

-- $ App Types
newtype AppType = AppType Text
  deriving (Show, Eq, Ord)

instance ToJSON AppType where
  toJSON (AppType t) = toJSON t

webApp :: AppType
webApp = AppType "web"
dbApp :: AppType
dbApp = AppType "db"
cacheApp :: AppType
cacheApp = AppType "cache"
workerApp :: AppType
workerApp = AppType "worker"
customApp :: AppType
customApp = AppType "custom"
rpcApp :: AppType
rpcApp = AppType "rpc"

data Service = Service
  { serviceApp :: !AppName
  , serviceAppType :: !AppType
  } deriving (Show)

instance ToJSON Service where
  toJSON Service{..} = object
    [ "app" .= serviceApp
    , "app_type" .= serviceAppType
    ]

newtype Traces = Traces [Trace]
  deriving (Show)

newtype Trace = Trace [Span]
  deriving (Show)

type Nanoseconds = Integer

newtype TraceId = TraceId Word64
  deriving (Show)

instance ToJSON TraceId where
   toJSON (TraceId i) = toJSON i

newtype SpanId = SpanId Word64
  deriving (Show)

instance ToJSON SpanId where
   toJSON (SpanId i) = toJSON i

data Span = Span
  { traceId :: !TraceId
  , spanId :: !SpanId
  , name :: !Text
  , resource :: !Text
  , service :: !AppName
  , spanType :: !SpanType
  , start :: !Nanoseconds
  , duration :: !Nanoseconds
  , parentId :: !(Maybe SpanId)
  , spanError :: !Bool
  , meta :: !(H.HashMap Text Text)
  } deriving (Show)

instance ToJSON Traces where
  toJSON (Traces ts) = toJSON ts

instance ToJSON Trace where
  toJSON (Trace ss) = toJSON ss

instance ToJSON Span where
  toJSON Span{..} = object $ mpid $ merror $ mmeta
    [ "trace_id" .= traceId
    , "span_id" .= spanId
    , "name" .= name
    , "resource" .= resource
    , "service" .= service
    , "type" .= spanType
    , "start" .= start
    , "duration" .= duration
    ]
    where
      mpid = maybe id (\pid -> (("parent_id" .= pid) :)) parentId
      merror = if spanError then (("error" .= (1 :: Int)) :) else id
      mmeta = if H.null meta then (("meta" .= meta) :) else id

data LifecycleSpan = Pending Span | Finished Span
  deriving (Show)

unLifecycle :: LifecycleSpan -> Span
unLifecycle (Pending s) = s
unLifecycle (Finished s) = s

data TraceState = TraceState
  { traceStateTraceId :: TraceId
  , spanIdGen :: AtomicCounter
  , finishedSpans :: IORef [Span]
  , parentSpan :: Maybe (IORef LifecycleSpan)
  , currentSpan :: Maybe (IORef LifecycleSpan)
  }

newTraceState
  :: MonadBase IO m
  => -- Text -- ^ Service type ()
  -- -> Text -- ^ Service name (name of the currently running app)
     TraceId
  -> m TraceState
newTraceState tid = liftBase $ do
  sid <- newCounter 0
  ss <- newIORef []
  return $ TraceState tid sid ss Nothing Nothing

data Context = Context
  { contextBaggage :: !(H.HashMap Text Text)
  , contextType :: !SpanType
  , contextService :: !AppName
  , contextResource :: !Text
  , contextName :: !Text
  }

ctxt :: SpanType -> AppName -> Text -> Text -> Context
ctxt = Context H.empty

class Monad m => MonadTrace m where
  getTraceId :: m TraceId
  getParentId :: m (Maybe SpanId)
  newSpanId :: m SpanId
  getSpan :: m (Maybe LifecycleSpan)

  registerCompletedSpan :: Span -> m ()

  modifySpan :: (Span -> Span) -> m ()

  descendIntoSpan :: IORef LifecycleSpan -> m a -> m a

instance (MonadBase IO m) => MonadTrace (ReaderT TraceState m) where
  getTraceId = traceStateTraceId <$> ask

  getParentId = do
    mpsRef <- parentSpan <$> ask
    forM mpsRef $ \psRef -> do
      ps <- readIORef psRef
      return $ spanId $ unLifecycle ps

  newSpanId = do
    spanGen <- spanIdGen <$> ask
    ident <- liftBase $ incrCounter 1 spanGen
    return $! SpanId (fromIntegral ident)

  getSpan = do
    msRef <- currentSpan <$> ask
    forM msRef readIORef

  registerCompletedSpan s = do
    st <- ask
    atomicModifyIORef' (finishedSpans st) (\ss -> s `seq` (s : ss, ()))

  modifySpan f = do
    msref <- currentSpan <$> ask
    forM_ msref $ \sref -> atomicModifyIORef' sref $ \ms -> case ms of
      Pending s -> (Pending $ f s, ())
      Finished s -> (Finished s, ())

  descendIntoSpan r = local (\st -> st { parentSpan = currentSpan st, currentSpan = Just r })

createSpan :: (MonadBase IO m, MonadTrace m) => Context -> m Span
createSpan Context{..} = do
  tid <- getTraceId
  sid <- newSpanId
  mpid <- fmap (spanId . unLifecycle) <$> getSpan
  startNanos <- nanos

  return $ Span
    { traceId = tid
    , spanId = sid
    , name = contextName
    , resource = contextResource
    , service = contextService
    , spanType = contextType
    , spanError = False
    , start = startNanos
    , duration = 0
    , parentId = mpid
    , meta = contextBaggage
    }

finalizeSpan :: (MonadBase IO m, MonadTrace m) => IORef LifecycleSpan -> m Span
finalizeSpan sref = do
  endNanos <- nanos
  s <- unLifecycle <$> readIORef sref
  let s' = s { duration = endNanos - start s }
  writeIORef sref $ Finished s
  return s'


nanos :: MonadBase IO m => m Nanoseconds
nanos = toNanoSecs <$> liftBase (getTime Realtime)

spanning :: (MonadBaseControl IO m, MonadTrace m) => Context -> m a -> m a
spanning c m = bracket
  (setUpSpan)
  (\sref -> finalizeSpan sref >>= registerCompletedSpan) $ \sref -> descendIntoSpan sref $ do
    r <- try m
    case r of
      Left err -> do
        -- TODO, should this spanning call be omitted?
        let cs = callStack
        modifySpan $ \s -> s
          { spanError = True
          , meta = meta s `H.union` H.fromList
            [ ("error.msg", pack $ show (err :: SomeException))
            , ("error.stack", pack $ prettyCallStack cs)
            ]
          }
        throw err
      Right ok -> return ok
    where
      setUpSpan = do
        s <- Pending <$> createSpan c
        sref <- newIORef s
        return sref


runTrace :: (MonadBase IO m) => TraceId -> ReaderT TraceState m a -> m (a, Trace)
runTrace tid m = do
  st <- newTraceState tid
  r <- runReaderT m st
  spans <- readIORef $ finishedSpans st
  return (r, Trace spans)


registerServices :: MonadBase IO m => H.HashMap Text Service -> m ()
registerServices ss = liftBase $ do
  m <- getGlobalManager
  req <- parseRequest "http://localhost:8126/v0.3/services"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode ss
        }
  resp <- httpNoBody req' m
  return $ responseBody resp

sendTrace :: (MonadBase IO m) => Trace -> m ()
sendTrace t = liftBase $ do
  m <- getGlobalManager
  req <- parseRequest "http://localhost:8126/v0.3/traces"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode $ Traces [t]
        }
  resp <- httpNoBody req' m
  return $ responseBody resp

requestDemo :: (MonadBaseControl IO m, MonadTrace m) => m [()]
requestDemo = spanning (ctxt webSpan "service_name" "/home" "request") $ do
  isAuthed <- spanning (ctxt webSpan "auth" "/check_manager" "check manager") $ return True

  comments <- if isAuthed
    then spanning (ctxt sqlSpan "postgresql" "betterteam_dev" "get comments") $ return [()]
    else return []

  return comments

-- $ Span Types
newtype SpanType = SpanType Text
  deriving (Show, Eq, Ord)

instance ToJSON SpanType where
  toJSON (SpanType t) = toJSON t

-- | Intended for HTTP clients, not web requests
httpSpan :: SpanType
httpSpan = SpanType "http"

webSpan :: SpanType
webSpan = SpanType "web"

sqlSpan :: SpanType
sqlSpan = SpanType "sql"

mongoSpan :: SpanType
mongoSpan = SpanType "mongo"

cassandraSpan :: SpanType
cassandraSpan = SpanType "cassandra"

queueSpan :: SpanType
queueSpan = SpanType "queue"

{-
-- $ Services

-- service: redis
-- component_name: redis-command

-- service: kafka
-- component: librdkafka
-- resource: Consume Topic _
--

-- $ Magic keys
-- span.type
-- service.name
-- resource.name
-- thread.name
-- thread.id
-- error.msg
-- error.type
-- error.stack

-- $ HTTP Meta constants
-- http.method
-- http.status_code
-- http.url

-- $ Net Meta constants
-- out.host
-- out.port

-- $ SQL constants
-- sql.query
-- db.name
-- db.user

-- $ System constants
-- system.pid

-- $ Tracer consts
-- lang: haskell
-- langversion: _
-- interpreter: _
-- lib_version: _

-- $ Redis consts
-- out.redis_db
-- redis.raw_command
-- redis.args_length
-- redis.pipeline_length
-- redis.pipeline_age
-- redis.pipeline_immediate_command
-}

-- $ HTTP headers one should set for distributed tracing.
-- These are cross-language (eg: Python, Go and other implementations should honor these)

hTraceId :: HeaderName
hTraceId = "x-datadog-trace-id"

hParentId :: HeaderName
hParentId = "x-datadog-parent-id"

hSamplingPriority :: HeaderName
hSamplingPriority = "x-datadog-sampling-priority"

samplingPriorityKey :: (IsString s) => s
samplingPriorityKey = "_sampling_priority_v1"
