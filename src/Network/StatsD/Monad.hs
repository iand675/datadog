{-# LANGUAGE MultiParamTypeClasses #-}

module Network.StatsD.Monad where

import Control.Monad.Except
import Control.Monad.Reader
import Network.StatsD.Datadog

class (Functor m, Applicative m, MonadIO m) => MonadStats m where
  getStatsClient :: m StatsClient

instance MonadStats m => MonadStats (ExceptT e m) where
  getStatsClient = lift getStatsClient

instance MonadStats m => MonadStats (ReaderT e m) where
  getStatsClient = lift getStatsClient

sendMetric :: MonadStats m => Metric -> m ()
sendMetric m = getStatsClient >>= flip sendSampled m

sendEvent :: MonadStats m => Event -> m ()
sendEvent e = getStatsClient >>= flip sendEvt e
