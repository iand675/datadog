{-# LANGUAGE MultiParamTypeClasses #-}
module Network.StatsD.Monad
where

import Control.Lens
import Control.Monad.IO.Class
import Network.StatsD.Datadog

class (Functor m, Applicative m, MonadIO m) => MonadStats m where
  getStatsClient :: m StatsClient

sendMetric :: MonadStats m => Metric -> m ()
sendMetric m = getStatsClient >>= flip sendSampled m

