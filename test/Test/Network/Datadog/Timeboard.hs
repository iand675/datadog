{-# LANGUAGE OverloadedStrings #-}

module Test.Network.Datadog.Timeboard (tests) where


import Control.Concurrent (threadDelay)
import Control.Exception
import Control.Lens hiding ((.=))

import Data.Aeson

import Distribution.TestSuite

import Network.Datadog (Environment, loadKeysFromEnv, createEnvironment)
import Network.Datadog.Timeboard hiding (name)


tests :: IO [Test]
tests = return
  [ Test TestInstance { run = testTimeboardCycle
                      , name = "Test timeboard CRUD"
                      , tags = ["Timeboard"]
                      , options = []
                      , setOption = \_ _ -> Left ""
                      }
  ]


environment :: IO Environment
environment = createEnvironment =<< loadKeysFromEnv


testTimeboardCycle :: IO Progress
testTimeboardCycle = do
  env <- environment
  let timeboardGraphDef = object ["requests" .= object ["q" .= ("system.cpu.idle{*} by {host}" :: String)]]
  let timeboardGraph = TimeboardGraph "Test Graph" timeboardGraphDef
  let timeboardDetails = TimeboardSpec "Test Timeboard" "No desc" [timeboardGraph] []
  let timeboardUpdatedDetails = timeboardDetails { timeboardSpecDescription = "New desc" }
  let computation = do
        threadDelay 500000
        timeboard1 <- createTimeboard env timeboardDetails
        threadDelay 500000
        timeboard2 <- updateTimeboard env (timeboard1 ^. id') timeboardUpdatedDetails
        threadDelay 500000
        timeboard3 <- loadTimeboard env (timeboard1 ^. id')
        threadDelay 500000
        timeboards <- loadTimeboards env
        threadDelay 500000
        deleteTimeboard env (timeboard1 ^. id')
        return (if timeboard2 /= timeboard3
                then Finished (Fail "Updated and fetched timeboards are not identical")
                else if timeboard2 `notElem` timeboards
                     then Finished (Fail (if timeboard1 `elem` timeboards
                                          then "Created timeboard not updated (in group load)"
                                          else "Updated timeboard not fetched from group load"))
                     else Finished Pass
               )
  catch computation (\e -> return $ Finished $ Fail $ show (e :: SomeException))
