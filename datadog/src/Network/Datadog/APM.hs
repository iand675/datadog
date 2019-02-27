{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE CPP #-}
module Network.Datadog.APM
  ( APMClient
  , localApmAgentUrl
  , mkApmClient
  , mkApmClient'
  -- * Monadic tracing
  , spanning
  , MonadTrace(..)
  , NoTraceT(..)
  -- * Low-level implementation support
  , sendTrace
  , MTrace(..)
  , Trace(..)
  , TraceState(spanStack)
  , createMutableTrace
  , completeMutableTrace
  , modifyMutableTrace
  , tagTrace
  , markTraceAsError
  , createMutableTraceSpan
  , completeMutableTraceSpan
  , Context(..)
  , ctxt
  , MSpan(..)
  , Span(..)
  , LifecycleSpan(..)
  , createSpan
  , finalizeSpan
  , httpSpan
  , webSpan
  , sqlSpan
  , mongoSpan
  , cassandraSpan
  , queueSpan
  , SpanType(..)
  , runTrace
  , webApp
  , dbApp
  , cacheApp
  , workerApp
  , customApp
  , rpcApp
  , AppName(..)
  , AppType(..)
  -- * Service Management
  , Service(..)
  , registerServices
  , Traces(..)
  , Nanoseconds
  , TraceId(..)
  , SpanId(..)
  -- * Request headers for interoperation
  , hTraceId
  , hParentId
  , hSamplingPriority
  -- * Miscellaneous
  , samplingPriorityKey
  ) where

import Control.Monad.Catch (MonadCatch, MonadThrow, MonadMask)
import Control.Monad.Fail
import Control.Exception (throw)
import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Control.Monad.Trans.Resource
import Control.Reaper
import Data.Aeson
import qualified Data.ByteString.Char8 as B
import Data.Conduit.Lazy (MonadActive)
import qualified Data.HashMap.Strict as H
import Data.String
import Data.Text (Text, pack)
import Data.Version
import Data.Word
import GHC.Stack
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Network.HTTP.Types.Header
import Paths_datadog
import System.Clock
import System.Random.MWC
import UnliftIO

data APMClient = APMClient
  { apmEndpoint :: IORef Request
  , apmReaper :: Reaper [Trace] Trace
  }

localApmAgentUrl :: String
localApmAgentUrl = "http://localhost:8126"

mkApmClient :: (MonadIO m, MonadThrow m) => String -> m APMClient
mkApmClient str = mkApmClient' str throw

mkApmClient' :: (MonadIO m, MonadThrow m) => String -> (HttpException -> IO ([Trace] -> [Trace])) -> m APMClient
mkApmClient' str errHandler = do
  baseReq <- parseRequest str
  ref <- newIORef baseReq
  let settings = defaultReaperSettings
        { reaperAction = \workload -> handle errHandler $ case splitAt 100 workload of
            (traces, rest) -> do
              m <- getGlobalManager
              req <- readIORef ref
              let req' = req
                    { method = "POST"
                    , path = "/v0.3/traces"
                    , requestBody = RequestBodyLBS $ encode $ Traces traces
                    , requestHeaders = baseHeaders ++ requestHeaders req
                    }
              _resp <- httpNoBody req' m
              -- TODO check resp here
              return (rest ++)
        }
  rpr <- liftIO $ mkReaper settings

  return $ APMClient
    { apmEndpoint = ref
    , apmReaper = rpr
    }


sendTrace :: MonadIO m => APMClient -> Trace -> m ()
sendTrace c = liftIO . reaperAdd (apmReaper c)

newtype AppName = AppName Text
  deriving (Show, Eq, Ord, IsString)

instance ToJSON AppName where
  toJSON (AppName t) = toJSON t

-- * App Types
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

instance Variate TraceId where
  uniform g = TraceId <$> uniformR (1, maxBound) g
  uniformR (TraceId l, TraceId h) g = TraceId <$> uniformR (max 1 l, h) g

newtype SpanId = SpanId Word64
  deriving (Show)

instance ToJSON SpanId where
   toJSON (SpanId i) = toJSON i

instance Variate SpanId where
  uniform g = SpanId <$> uniformR (1, maxBound) g
  uniformR (SpanId l, SpanId h) g = SpanId <$> uniformR (max 1 l, h) g

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
      mmeta = if H.null meta then id else (("meta" .= meta) :)

-- | A mutable span. Once the span has been finalized, subsequent modification attempts are ignored.
newtype MSpan = MSpan { fromMSpan :: IORef LifecycleSpan }

-- | A span which is tagged based on whether it is incomplete or finished.
data LifecycleSpan = Pending Span | Finished Span
  deriving (Show)

unLifecycle :: LifecycleSpan -> Span
unLifecycle (Pending s) = s
unLifecycle (Finished s) = s

data MTrace
  = MTrace TraceState
  | Dummy -- ^ All actions taken upon the current mutable trace will be discarded if performed upon a dummy trace.

-- | The current state of a mutable trace.
data TraceState = TraceState
  { traceStateTraceId :: TraceId
  , spanIdGen :: GenIO
  , finishedSpans :: IORef [Span]
  , spanStack :: [MSpan] -- Stack of parent spans
  }

-- | Create a mutable trace. While the trace is not completed, additional data may be added and modified.
-- This includes adding child spans.
--
-- It is generally recommended to use the 'MonadTrace' interface aside from creating root level traces with this function.
createMutableTrace
  :: MonadIO m
  => TraceId
  -> GenIO
  -> m MTrace
createMutableTrace tid gen = do
  ss <- newIORef []
  return $ MTrace $ TraceState tid gen ss []

completeMutableTrace :: (MonadIO m) => MTrace -> m Trace
completeMutableTrace Dummy = return $ Trace []
completeMutableTrace (MTrace st) = do
  spans <- readIORef $ finishedSpans st
  return $ Trace spans

-- | Modify the active span in the trace. Any data within the provided span may be modified, so this function is very powerful. It is recommended to use the monadic interface, which correctly manages the lifecycle of spans.
--
-- It is generally recommended to use the 'MonadTrace' interface aside from creating root level traces with this function.
modifyMutableTrace :: (MonadIO m) => MTrace -> (Span -> Span) -> m ()
modifyMutableTrace mt f = do
  forM_ (stackHead mt) $ \sref -> atomicModifyIORef' (fromMSpan sref) $ \ms -> case ms of
    Pending s -> (Pending $ f s, ())
    Finished s -> (Finished s, ())

-- | Add an unordered map of distinct key/value pairs to the trace
tagTrace :: (MonadUnliftIO m) => MTrace -> H.HashMap Text Text -> m ()
tagTrace st hm = case stackHead st of
  Nothing -> return ()
  Just sref -> modifyIORef' (fromMSpan sref) $ \ls -> case ls of
    Pending s -> Pending $! s { meta = meta s `H.union` hm }
    Finished _ -> ls

markTraceAsError :: (MonadUnliftIO m) => MTrace -> m ()
markTraceAsError mt = case stackHead mt of
  Nothing -> return ()
  Just sref -> modifyIORef' (fromMSpan sref) $ \ls -> case ls of
    Pending s -> Pending $! s { spanError = True }
    Finished _ -> ls

stackHead :: MTrace -> Maybe MSpan
stackHead Dummy = Nothing
stackHead (MTrace st) = case spanStack st of
  [] -> Nothing
  (s:_) -> Just s

createMutableTraceSpan :: (MonadIO m) => MTrace -> Context -> m MTrace
createMutableTraceSpan Dummy _ = return Dummy
createMutableTraceSpan mtrace@(MTrace st) Context{..} = do
  sid <- liftIO $ uniform $ spanIdGen st
  startNanos <- nanos
  mpid <- forM (stackHead mtrace) $ \sref -> (spanId . unLifecycle) <$> readIORef (fromMSpan sref)

  let theSpan = Span
        { traceId = traceStateTraceId st
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

  spanRef <- newIORef (Pending theSpan)
  return $ MTrace $ st
    { spanStack = MSpan spanRef : spanStack st
    }

completeMutableTraceSpan :: MonadUnliftIO m => MTrace -> m MTrace
completeMutableTraceSpan Dummy = return Dummy
completeMutableTraceSpan mtrace@(MTrace st) = case stackHead mtrace of
  Nothing -> return mtrace
  Just sref -> do
    endNanos <- nanos
    finalizedSpan <- atomicModifyIORef' (fromMSpan sref) $ \ls ->
      let s = unLifecycle ls
          s' = s { duration = endNanos - start s }
      in (Finished s', s')
    atomicModifyIORef' (finishedSpans st) $ \ss -> (finalizedSpan : ss, ())
    return $ MTrace $ st
      { spanStack = case spanStack st of
          [] -> []
          (_:ps) -> ps
      }

data Context = Context
  { contextBaggage :: !(H.HashMap Text Text)
  , contextType :: !SpanType
  , contextService :: !AppName
  , contextResource :: !Text
  , contextName :: !Text
  }

ctxt :: SpanType -> AppName -> Text -> Text -> Context
ctxt = Context H.empty

class MonadUnliftIO m => MonadTrace m where
  {-# MINIMAL currentTrace, descendIntoSpan #-}
  -- | Get the current trace in the context.
  currentTrace :: m MTrace

  -- | Get the current trace's id.
  getTraceId :: m TraceId
  getTraceId = do
    mt <- currentTrace
    return $ case mt of
      Dummy -> TraceId 0
      (MTrace st) -> traceStateTraceId st

  -- | Get the id of the parent span. If this span is a root-level span, returns 'Nothing'.
  getParentId :: m (Maybe SpanId)
  getParentId =  do
    mpsRef <- stackHead <$> currentTrace
    forM mpsRef $ \psRef -> do
      ps <- readIORef $ fromMSpan psRef
      return $ spanId $ unLifecycle ps

  -- | Generate a new 'SpanId'. This does not create a full new span, only a random id.
  newSpanId :: m SpanId
  newSpanId = do
    mt <- currentTrace
    case mt of
      Dummy -> return $ SpanId 0
      (MTrace st) -> liftIO $ uniform $ spanIdGen st

  -- | Get the current span in the context.
  getSpan :: m (Maybe LifecycleSpan)
  getSpan = do
    msRef <- stackHead <$> currentTrace
    forM msRef (readIORef . fromMSpan)

  -- | Add the current span to the list of finished spans.
  registerCompletedSpan :: Span -> m ()
  registerCompletedSpan s = do
    mt <- currentTrace
    case mt of
      Dummy -> return ()
      (MTrace st) -> atomicModifyIORef' (finishedSpans st) (\ss -> s `seq` (s : ss, ()))

  -- | Pureley alter the current span.
  modifySpan :: (Span -> Span) -> m ()
  modifySpan f = do
    mt <- currentTrace
    modifyMutableTrace mt f

  -- | Push a new span onto the stack. This is primarily intended for writing new 'MonadTrace' instances.
  --
  -- Most implementations should use something like the @MonadReader@ 'local' function:
  --
  -- > instance (MonadUnliftIO m) => MonadTrace (ReaderT MTrace m) where
  -- >   currentTrace = ask
  -- >   descendIntoSpan r = local $ \st -> case st of
  -- >     Dummy -> st
  -- >     (MTrace innerSt) -> MTrace $ innerSt
  -- >      { spanStack = r : spanStack innerSt
  -- >      }
  descendIntoSpan :: MSpan -> m a -> m a

instance (MonadUnliftIO m) => MonadTrace (ReaderT MTrace m) where
  currentTrace = ask
  descendIntoSpan r = local $ \st -> case st of
    Dummy -> st
    (MTrace innerSt) -> MTrace $ innerSt
      { spanStack = r : spanStack innerSt
      }

instance MonadTrace m => MonadTrace (ResourceT m) where
  currentTrace = lift currentTrace
  descendIntoSpan r = transResourceT (descendIntoSpan r)

-- | An adapter transformer for dropping all trace data.
--
-- Any 'MonadTrace' actions run in the 'NoTraceT' monadic context will discard any tracing details.
newtype NoTraceT m a = NoTraceT
  { runNoTraceT :: m a
  } deriving ( Monad
             , Functor
             , Applicative
             , MonadIO
             , MonadResource
             , MonadActive
             , MonadThrow
             , MonadCatch
             , MonadMask
             , MonadFail
             )

instance MonadUnliftIO m => MonadUnliftIO (NoTraceT m) where
  askUnliftIO = NoTraceT $ do
    (UnliftIO ui) <- askUnliftIO
    return $ UnliftIO (ui . runNoTraceT)

instance MonadTrans NoTraceT where
  lift = NoTraceT

deriving instance MonadError e m => MonadError e (NoTraceT m)
deriving instance MonadReader r m => MonadReader r (NoTraceT m)
deriving instance MonadWriter w m => MonadWriter w (NoTraceT m)
deriving instance MonadState s m => MonadState s (NoTraceT m)

instance (MonadUnliftIO m) => MonadTrace (NoTraceT m) where
  currentTrace = return Dummy
  descendIntoSpan _ = id

-- | Create a span for the given context
createSpan :: (MonadUnliftIO m, MonadTrace m) => Context -> m Span
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

finalizeSpan :: (MonadUnliftIO m, MonadTrace m) => MSpan -> m Span
finalizeSpan sref = do
  endNanos <- nanos
  s <- unLifecycle <$> readIORef (fromMSpan sref)
  let s' = s { duration = endNanos - start s }
  writeIORef (fromMSpan sref) $ Finished s
  return s'


nanos :: MonadIO m => m Nanoseconds
nanos = toNanoSecs <$> liftIO (getTime Realtime)

-- | The primary function of the APM module. Performing actions within a call to
-- 'spanning' will record timing details, error details, and stack traces. Additional
-- metadata may be added via modifying the span via the 'modifySpan' function.
spanning :: (MonadUnliftIO m, MonadTrace m) => Context -> m a -> m a
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
        throwIO err
      Right ok -> return ok
    where
      setUpSpan = do
        s <- Pending <$> createSpan c
        sref <- newIORef s
        return $ MSpan sref

-- | A minimal implementation for running an action and returning trace data for it.
runTrace :: (MonadUnliftIO m) => TraceId -> GenIO -> ReaderT MTrace m a -> m (a, Trace)
runTrace tid gen m = do
  st <- createMutableTrace tid gen
  r <- runReaderT m st
  spans <- completeMutableTrace st
  return (r, spans)


registerServices :: (MonadIO m) => H.HashMap Text Service -> m ()
registerServices ss = liftIO $ do
  m <- getGlobalManager
  req <- parseRequest "http://localhost:8126/v0.3/services"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode ss
        }
  resp <- httpNoBody req' m
  return $ responseBody resp

baseHeaders :: [Header]
baseHeaders =
  [ ("Datadog-Meta-Lang", "Haskell")
  -- , ("Datadog-Meta-Lang-Version", "ghc-__GLASGOW_HASKELL__")
  -- , ("Datadog-Meta-Lang-Interpreter", "ghc-__GLASGOW_HASKELL__-arch_HOST_ARCH-GOOS")
  , ("Datadog-Meta-Tracer-Version", B.pack $ showVersion version)
  -- ("Content-Type", "application/json; charset=utf-8")
  ]

-- * Non-bracketable spans (needed for things like wai-middleware)


-- * Span Types

-- | Constructor exposed so that custom span types may be used, but datadog has somewhat "magic" UI support
-- for certain span types, such as "sql", so it is recommended to use the official 'SpanType's where possible.
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
