{-# LANGUAGE QuasiQuotes     #-}
{-# LANGUAGE RecordWildCards #-}

module API.Requests
  ( getLongPollingServer
  , longPoll
  , sendMessage
  , parseResponse -- needed for testing
  ) where

import BotPrelude

import API.Types (Error(..), LongPollResponse(..), Failed, LongPollServerSettings(..), key, server, ts, MessageId(..))
import qualified API.Types.Utils as Utils (prettifyError)

import Bot.Config   (longPollVersion)
import Bot.Types    (Bot, config)
import Data.Aeson   (decode)
import Network.Wreq (param)

-- TODO: import as qualified Log and rename functions
import Service.Logging     (logError)
import qualified Service.UrlComposer as Url (messagesGetLongPollServer, messagesSend, mkLongPollServerUrl)
import qualified Service.Wreq as Wreq (getWith)

import Data.ByteString.Lazy as LBS
import Data.Text.Encoding   as T

import NeatInterpolation

getLongPollingServer :: Bot (Maybe LongPollServerSettings)
getLongPollingServer = do
  lp_version <- (^. (config . longPollVersion)) <$> ask
  let url = Url.messagesGetLongPollServer
  json <- Wreq.getWith url (patch lp_version)
  maybe (pure Nothing) (parse url) json
    where
      patch ver o = o & param "lp_version" .~ [ver]

longPoll :: LongPollServerSettings -> Bot (Either Failed (Maybe LongPollResponse))
longPoll settings = do
  version <- (^. (config . longPollVersion)) <$> ask
  let url = Url.mkLongPollServerUrl (settings ^. server)
  json <- Wreq.getWith url (patch version)

  failed <- maybe (pure Nothing) (parse url) json :: _ (Maybe Failed)

  case failed of 
    Just f -> pure $ Left f
    Nothing -> 
      pure $ Right $ do
        maybe (pure Nothing) (parse url) json
    where
       -- https://vk.com/dev/using_longpoll
      patch version o = o
         & param "act" .~ ["a_check"]
         & param "mode" .~ ["2"]
         & param "wait" .~ ["25"]
         & param "version" .~ [version]
         & param "key" .~ [settings ^. key]
         & param "ts" .~ [showT $ settings ^. ts]

sendMessage :: Integer -> Text -> Bot (Maybe MessageId)
sendMessage userId msg = do
  let url = Url.messagesSend
  -- rand <- lift $ (randomIO :: IO Integer)
  result <- Wreq.getWith url patch -- $ showT rand)
  maybe (pure Nothing) (parse url) result
    where 
      patch o = o
        & param "user_id" .~ [showT userId]
        & param "peer_id" .~ [showT userId]
        & param "message" .~ [msg]
        -- & param "random_id" .~ [r]

parse :: FromJSON a => Text -> LBS.ByteString -> Bot (Maybe a)
parse methodName bs = case parseResponse bs of
    Left ParsingError{..} -> do
        logError [text|'Request: ${methodName}'
          Failed to parse JSON:
          ${_json}|]
        pure Nothing
    Left e@Error{..} -> do
        let source = Utils.prettifyError e
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
