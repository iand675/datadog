{-# LANGUAGE OverloadedStrings #-}

{-|
Timeboards display tyme-synchronized graphs of metrics and events.
-}
module Network.Datadog.Timeboard
( TimeboardSpec(..)
, TimeboardGraph(..)
, TimeboardVariable(..)
, Timeboard(..)
, TimeboardId
, createTimeboard
, updateTimeboard
, deleteTimeboard
, loadTimeboard
, loadTimeboards
, HasId'(..)
, HasSpec(..)
, HasTitle(..)
, HasDescription(..)
, HasGraphs(..)
, HasVariables(..)
, HasName(..)
, HasPrefix(..)
, HasDefault'(..)
, HasDefinition(..)
) where


import Control.Monad (liftM, void)

import Data.Aeson (encode)

import Network.HTTP.Types

import Network.Datadog.Internal


-- | Create a new timeboard in Datadog matching a specification.
createTimeboard :: Environment -> TimeboardSpec -> IO Timeboard
createTimeboard env timeboardspec =
  let path = "dash"
  in datadogHttp env path [] POST (Just $ encode timeboardspec) >>=
     decodeDatadog "createTimeboard"


-- | Load a timeboard from Datadog by its ID.
loadTimeboard :: Environment -> TimeboardId -> IO Timeboard
loadTimeboard env timeboardId =
  let path = "dash/" ++ show timeboardId
  in liftM wrappedTimeboard $
     datadogHttp env path [] GET Nothing >>=
     decodeDatadog "loadTimeboard"


-- | Sync a timeboard with Datadog.
updateTimeboard :: Environment -> TimeboardId -> TimeboardSpec -> IO Timeboard
updateTimeboard env timeboardId mspec =
  let path = "dash/" ++ show timeboardId
  in datadogHttp env path [] PUT (Just $ encode mspec) >>=
     decodeDatadog "updateTimeboard"


-- | Delete a timeboard from Datadog.
deleteTimeboard :: Environment -> TimeboardId -> IO ()
deleteTimeboard env timeboardId =
  let path = "dash/" ++ show timeboardId
  in void $ datadogHttp env path [] DELETE Nothing


-- | Load all timeboard from Datadog.
loadTimeboards :: Environment -> IO [Timeboard]
loadTimeboards env =
  let path = "dash"
  in liftM wrappedTimeboards $
     datadogHttp env path [] GET Nothing >>=
     decodeDatadog "loadTimeboards"
