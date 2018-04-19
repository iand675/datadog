{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
module Network.Datadog.APM where

import Control.Exception.Lifted
import Control.Monad.Base
import Control.Monad.Reader
import Control.Monad.Trans.Control
import Data.Aeson
import qualified Data.ByteString.Lazy as L
import Data.Atomics.Counter
import qualified Data.HashMap.Strict as H
import Data.IORef.Lifted
import Data.Text (Text, pack)
import Data.Word
import GHC.Stack
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import System.Clock

data Service = Service
  { serviceApp :: !Text
  , serviceAppType :: !Text
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
  , service :: !Text
  , spanType :: !Text
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

newTraceState :: MonadBase IO m => TraceId -> m TraceState
newTraceState tid = liftBase $ do
  sid <- newCounter 0
  ss <- newIORef []
  return $ TraceState tid sid ss Nothing Nothing

data Context = Context
  { contextBaggage :: !(H.HashMap Text Text)
  , contextType :: !Text
  , contextService :: !Text
  , contextResource :: !Text
  , contextName :: !Text
  }

ctxt :: Text -> Text -> Text -> Text -> Context
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
            [ ("error.exception", pack $ show (err :: SomeException))
            , ("error.trace", pack $ prettyCallStack cs)
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


registerServices :: MonadBase IO m => H.HashMap Text Service -> m L.ByteString
registerServices ss = liftBase $ do
  m <- getGlobalManager
  req <- parseRequest "http://localhost:8126/v0.3/services"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode ss
        }
  resp <- httpLbs req' m
  return $ responseBody resp

sendTrace :: (MonadBase IO m) => Trace -> m L.ByteString
sendTrace t = liftBase $ do
  m <- getGlobalManager
  req <- parseRequest "http://localhost:8126/v0.3/traces"
  let req' = req
        { method = "POST"
        , requestBody = RequestBodyLBS $ encode $ Traces [t]
        }
  resp <- httpLbs req' m
  return $ responseBody resp

requestDemo :: (MonadBaseControl IO m, MonadTrace m) => m [()]
requestDemo = spanning (ctxt "web" "service_name" "/home" "request") $ do
{-
  isAuthed <- spanning (ctxt "web" "auth" "/check_manager" "check manager") $ return True

  comments <- if isAuthed
    then spanning (ctxt "sql" "postgresql" "betterteam_dev" "get comments") $ return [()]
    else return []
-}

  return [()]
