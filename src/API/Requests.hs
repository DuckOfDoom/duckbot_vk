{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module API.Requests
  ( getLongPollingServer
  , longPoll
  , parseResponse -- needed for testing
  ) where

import BotPrelude

import API.Types (Error(..), LongPollResponse(..), LongPollServerSettings(..),
                  Update(..), key, prettifyError, server, ts)

import Bot.Config   (longPollVersion)
import Bot.Types    (Bot, config)
import Data.Aeson   (decode)
import Network.Wreq (param)

import Service.Logging     (logError)
import Service.UrlComposer as Url (getLongPollServer, mkLongPollServerUrl)
import Service.Wreq        (getWith)

import Data.ByteString.Lazy as LBS
import Data.Text.Encoding   as T

import NeatInterpolation

getLongPollingServer :: Bot (Maybe LongPollServerSettings)
getLongPollingServer = do
  lp_version <- (^. (config . longPollVersion)) <$> ask
  json <- getWith Url.getLongPollServer (patch lp_version)
  maybe (pure Nothing) (parse Url.getLongPollServer) json
    where
      patch ver o = o & param "lp_version" .~ [ver]

longPoll :: LongPollServerSettings -> Bot (Maybe LongPollResponse)
longPoll settings = do
  version <- (^. (config . longPollVersion)) <$> ask
  let serverUrl = Url.mkLongPollServerUrl (settings ^. server)
  json <- getWith serverUrl (patch version)
  maybe (pure Nothing) (parse serverUrl) json
    where
       -- https://vk.com/dev/using_longpoll
      patch version o = o
         & param "act" .~ ["a_check"]
         & param "mode" .~ ["2"]
         & param "wait" .~ ["25"]
         & param "version" .~ [version]
         & param "key" .~ [settings ^. key]
         & param "ts" .~ [showT $ settings ^. ts]

parse :: FromJSON a => Text -> LBS.ByteString -> Bot (Maybe a)
parse methodName bs = case parseResponse bs of
    Left ParsingError{..} -> do
        logError [text|'Request: ${methodName}'
          Failed to parse JSON:
          ${_json}|]
        pure Nothing
    Left e@Error{..} -> do
        let source = prettifyError e
        logError [text|'Request ${methodName}'
          Received error:
          ${source}|]
        pure Nothing
    Right result -> pure $ Just result

parseResponse :: FromJSON a => LBS.ByteString -> Either Error a
parseResponse bs =
    case (decode bs) :: Maybe Error of
      Just err  -> Left err
      Nothing ->
        case decode bs of
          Just r  -> Right r
          Nothing -> Left parsingError
    where
      parsingError = ParsingError "Failed to parse JSON" (T.decodeUtf8 $ LBS.toStrict bs)
