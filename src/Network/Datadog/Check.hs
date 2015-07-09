{-# LANGUAGE OverloadedStrings #-}

{-|
Checks allow users to post check statuses, for use with monitors.
-}
module Network.Datadog.Check
( CheckStatus(..)
, CheckResult(..)
, recordCheck
, HasStatus(..)
, HasHostName(..)
, HasCheck(..)
, HasTimestamp(..)
, HasTags(..)
, HasMessage(..)
, AsCheckStatus(..)
,
) where

import Control.Monad (void)
import Data.Aeson (encode)
import Network.HTTP.Types
import Network.Datadog.Internal

-- | Record the result of a check in Datadog.
recordCheck :: Environment -> CheckResult -> IO ()
recordCheck env checkResult =
  let path = "check_run"
  in void $ datadogHttp env path [] POST $ Just $ encode checkResult

