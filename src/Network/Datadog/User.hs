{-# LANGUAGE OverloadedStrings #-}

{-|
Users in Datadog.
-}

module Network.Datadog.User
( DatadogUser(userHandle)
, UserHandle
, UserAccessRole
, newUser
, updateUser
, disableUser
, loadUser
, loadUsers
) where

import Control.Monad (void)
import Data.Aeson
import Network.HTTP.Types
import Network.Datadog.Internal

-- | Store a new user in Datadog.
newUser :: UserHandle -> NewUser
newUser email = NewUser email Nothing Nothing

-- | Update a user in Datadog
updateUser :: Environment -> UserHandle -> UserAccessRole -> IO DatadogUser
updateUser env uid uar =
  let path = "user/" ++ show uid
  in datadogHttp env path [] PUT (Just $ encode uar) >>=
     decodeDatadog "updateUser"

-- | Disable a user in Datadog
disableUser :: Environment -> UserHandle -> IO ()
disableUser env uid =
  let path = "user/" ++ show uid
  in void $ datadogHttp env path [] DELETE Nothing

-- | Load a user from Datadog by its handle.
loadUser :: Environment -> UserHandle -> IO DatadogUser
loadUser env userHandle =
  let path = "user/" ++ show userHandle
  in datadogHttp env path [] GET Nothing >>=
     decodeDatadog "loadUser"

-- | Load all users from a given org
loadUsers :: Environment -> IO [DatadogUser]
loadUsers env =
    let path = "user"
    in datadogHttp env path [] GET Nothing >>=
       decodeDatadog "loadUsers"
