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

import Network.HTTP.Conduit

import Network.Datadog


muteHost :: Environment -> Text -> Maybe UTCTime -> Bool -> IO ()
-- ^ Do not allow alerts to trigger on a specific host
muteHost (Environment keys manager) hostname mtime override = do
  let body = object (["hostname" .= hostname]
                     ++ maybe [] (\a -> ["end" .= (ceiling (utcTimeToPOSIXSeconds a) :: Integer)]) mtime
                     ++ ["override" .= True | override]
                    )
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/host/" ++ unpack hostname
             ++ "/mute?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
             ++ if override then "&override=true" else ""
  let request = initReq { method = "POST"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode body)
                        }
  void $ httpLbs request manager


unmuteHost :: Environment -> Text -> IO ()
-- ^ Allow alerts to trigger on a specific host
unmuteHost (Environment keys manager) hostname = do
  let body = object ["hostname" .= hostname]
  initReq <- parseUrl $ "https://app.datadoghq.com/api/v1/host/" ++ unpack hostname
             ++ "/unmute?api_key=" ++ apiKey keys ++ "&application_key=" ++ appKey keys
  let request = initReq { method = "POST"
                        , requestHeaders = [("Content-type","application/json")]
                        , requestBody = RequestBodyLBS (encode body)
                        }
  void $ httpLbs request manager
