{-# LANGUAGE OverloadedStrings #-}

{-|
Users in Datadog.
-}

module Network.Datadog.User
( UserSpec(..)
, DatadogUser(userHandle, userDetails)
, UserHandle
, minimalUserSpec
, newUser
, loadUser
, loadUsers
) where

import Control.Monad (liftM)
import Control.Monad (void)

import Data.Aeson hiding (Error, Success)
-- import qualified Data.Aeson (Result(Success))
import Data.Text (Text)

import Network.HTTP.Types

import Network.Datadog.Internal



-- | Store a new user in Datadog.
newUser :: UserHandle -> NewUser
newUser email = NewUser email Nothing Nothing


-- | Load a user from Datadog by its handle.
loadUser :: Environment -> UserHandle -> IO ()
loadUser env userHandle = 
  let path = "users/" ++ show userHandle
  in void $ datadogHttp env path [] POST Nothing