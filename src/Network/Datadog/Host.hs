{-# LANGUAGE OverloadedStrings #-}

{-|
Controls host muting in Datadog.
-}
module Network.Datadog.Host
( muteHost
, unmuteHost
) where


import Control.Monad (void)

import Data.Aeson
import Data.Text (Text, unpack)
import Data.Time.Clock
import Data.Time.Clock.POSIX

import Network.HTTP.Types

import Network.Datadog.Internal


muteHost :: Environment -> Text -> Maybe UTCTime -> Bool -> IO ()
-- ^ Do not allow alerts to trigger on a specific host
muteHost env hostname mtime override =
  let path = "host/" ++ unpack hostname ++ "/mute"
      q = [("override", "true") | override]
      body = object $
             prependMaybe (\a -> "end" .= (ceiling (utcTimeToPOSIXSeconds a) :: Integer)) mtime $
             prependBool override ("override" .= True)
             ["hostname" .= hostname]
  in void $ datadogHttp env path q POST $ Just $ encode body


unmuteHost :: Environment -> Text -> IO ()
-- ^ Allow alerts to trigger on a specific host
unmuteHost env hostname =
  let path = "host/" ++ unpack hostname ++ "/unmute"
      body = object ["hostname" .= hostname]
  in void $ datadogHttp env path [] POST $ Just $ encode body
