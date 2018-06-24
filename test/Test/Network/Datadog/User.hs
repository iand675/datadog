module Test.Network.Datadog.User (spec) where

import Control.Concurrent (threadDelay)
import Control.Lens ((^.))
import Control.Monad (when)
import Test.Hspec (Spec, describe, expectationFailure, it)

import Network.Datadog (Environment, createEnvironment, loadKeysFromEnv)
import Network.Datadog.User
  ( newUser
  , updateUser
  , disableUser
  , loadUser
  , loadUsers
  )

spec :: Spec
spec = describe "User operations" $ do
  it "Does user CRUD operations" $ do
    let environment :: IO Environment
        environment = createEnvironment =<< loadKeysFromEnv
    env <- environment
    let userEmail = "test@example.com"
    user1 <- newUser userEmail
    -- load an existing user
    user2 <- loadUser env (user1 ^. handle)
    when (user1 /= user2) $
      expectationFailure "Failed"