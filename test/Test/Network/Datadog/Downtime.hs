module Test.Network.Datadog.Downtime (spec) where

import Control.Concurrent (threadDelay)
import Control.Lens ((&), (?~), (^.))
import Control.Monad (when)
import Data.Time.Clock (addUTCTime, getCurrentTime)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Downtime
  ( cancelDowntime
  , end
  , id'
  , loadDowntime
  , loadDowntimes
  , minimalDowntimeSpec
  , scheduleDowntime
  , updateDowntime
  )

spec :: Spec
spec = describe "Downtime spec" $ do
  it "Does downtime CRUD operations" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
        downtimeDetails = minimalDowntimeSpec $ read "haskell-datadog-test-scope"
    env <- environment
    time <- getCurrentTime
    let downtimeUpdatedDetails = downtimeDetails & end ?~ addUTCTime 300 time
    downtime1 <- scheduleDowntime env downtimeDetails
    threadDelay 500000
    downtime2 <- updateDowntime env (downtime1 ^. id') downtimeUpdatedDetails
    threadDelay 500000
    downtime3 <- loadDowntime env (downtime1 ^. id')
    threadDelay 500000
    downtimes <- loadDowntimes env False
    threadDelay 500000
    cancelDowntime env (downtime1 ^. id')
    when (downtime2 /= downtime3) $
      expectationFailure "Updated and fetched downtimes are not identical"
    when (downtime2 `notElem` downtimes) $
      expectationFailure "Created downtime not updated (in group load)"
