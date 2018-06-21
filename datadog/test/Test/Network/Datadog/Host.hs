module Test.Network.Datadog.Host (spec) where

import Control.Concurrent (threadDelay)
import Data.Semigroup ((<>))
import Data.Text (Text, pack)
import Data.Time.Clock (UTCTime, addUTCTime, getCurrentTime)
import System.Random (randomIO)
import Test.Hspec (Spec, describe, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.Host (muteHost, unmuteHost)

performMute :: Environment -> Text -> Maybe UTCTime -> Bool -> IO ()
performMute env host time override = do
  threadDelay 1000000
  muteHost env host time override

performUnmute :: Environment -> Text -> IO ()
performUnmute env host = do
  threadDelay 1000000
  unmuteHost env host

spec :: Spec
spec = describe "Host spec" $ do
  it "Mutes and unmutes hosts" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
    env <- environment
    time <- getCurrentTime
    host <- ("haskell-datadog-test-host-" <>) . pack . show <$> (randomIO :: IO Word)
    -- Ensure overriding works
    performMute env host Nothing True
    performMute env host (Just (addUTCTime 30 time)) True
    -- Unmute the host before we test no override option
    performUnmute env host
    performMute env host Nothing False
    -- Unmute for "cleanup"
    performUnmute env host
