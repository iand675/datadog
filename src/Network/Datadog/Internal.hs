{-# LANGUAGE OverloadedStrings #-}

module Network.Datadog.Internal
( prependMaybe
, prependBool
, datadogHttp
, decodeDatadog
) where


import Control.Exception

import Data.Aeson
import qualified Data.ByteString as BS (ByteString)
import qualified Data.ByteString.Lazy as LBS (ByteString, empty)
import Data.Maybe
import Data.Text (pack)
import Data.Text.Lazy (unpack)
import Data.Text.Encoding (encodeUtf8)
import Data.Text.Lazy.Encoding (decodeUtf8)

import Network.HTTP.Client

import Network.Datadog


prependMaybe :: (a -> b) -> Maybe a -> [b] -> [b]
prependMaybe _ Nothing = id
prependMaybe f (Just a) = (f a :)


prependBool :: Bool -> b -> [b] -> [b]
prependBool p a = if p then (a :) else id


datadogHttp :: Environment-> String -> [(String, String)] -> BS.ByteString -> Maybe LBS.ByteString -> IO LBS.ByteString
datadogHttp (Environment keys baseUrl manager) endpoint query httpMethod content = do
  initReq <- parseUrl $ baseUrl ++ endpoint
  let body = RequestBodyLBS $ fromMaybe LBS.empty content
  let headers = [("Content-type", "application/json") | isJust content]
  let apiQuery = [("api_key", apiKey keys)
                 ,("application_key", appKey keys)]
  let fullQuery = map (\(a,b) -> (encodeUtf8 (pack a), Just (encodeUtf8 (pack b)))) $
                  apiQuery ++ query
  let request = setQueryString fullQuery $
                initReq { method = httpMethod
                        , requestBody = body
                        , requestHeaders = headers
                        }
  responseBody <$> httpLbs request manager


decodeDatadog :: FromJSON a => String -> LBS.ByteString -> IO a
decodeDatadog funcname body = either (throwIO . AssertionFailed . failstring) return $
                              eitherDecode body
  where failstring e = "Datadog Library decoding failure in \"" ++ funcname ++
                       "\": " ++ e ++ ": " ++ unpack (decodeUtf8 body)
