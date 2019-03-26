module VK.Requests
  ( getLongPollingServer
  , longPoll
  , sendMessage
  , sendMessageWithKeyboard
  , eitherParse -- needed for testing
  ) where

import BotPrelude

import           VK.Types       (Error(..), Failed, Keyboard(..),
                                 LongPollResponse(..),
                                 LongPollServerSettings(..), MessageId(..), key,
                                 server, ts)
import qualified VK.Types.Utils as Utils (prettifyError)

import Bot.Config   (accessToken, apiVersion, longPollVersion)
import Bot.Types    (Bot, config, lastSentMessageId, updateStateForUser)
import Data.Aeson   (decode)
import Network.Wreq (param)

import qualified Service.Logging     as Log (error)
import qualified Service.UrlComposer as Url (messagesGetLongPollServer,
                                             messagesSend, mkLongPollServerUrl)
import qualified Service.Wreq        as Wreq (Options, defaults, getWith)

import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text.Encoding   as TE
import qualified Data.Text as T
import qualified NeatInterpolation as F

defaultOpts :: Bot Wreq.Options
defaultOpts = do
  env <- ask
  pure $ Wreq.defaults
    & param "access_token" .~ [env ^. (config . accessToken)]
    & param "v"            .~ [env ^. (config . apiVersion)]

getLongPollingServer :: Bot (Maybe LongPollServerSettings)
getLongPollingServer = do
  lp_version <- (^. (config . longPollVersion)) <$> ask
  let url = Url.messagesGetLongPollServer

  opts <- defaultOpts
   <&> param "lp_version" .~ [lp_version]

  json <- Wreq.getWith url opts
  maybe (pure Nothing) (parse url) json

longPoll :: LongPollServerSettings -> Bot (Either Failed (Maybe LongPollResponse))
longPoll settings = do
  version <- (^. (config . longPollVersion)) <$> ask
  let url = Url.mkLongPollServerUrl (settings ^. server)

  opts <- defaultOpts
    <&> param "act" .~ ["a_check"]
    <&> param "mode" .~ ["2"]
    <&> param "wait" .~ ["25"]
    <&> param "version" .~ [version]
    <&> param "key" .~ [settings ^. key]
    <&> param "ts" .~ [showT $ settings ^. ts]

  json <- Wreq.getWith url opts
  failed <- maybe (pure Nothing) parseSilent json :: Bot (Maybe Failed)
  case failed of
    Just f -> pure $ Left f
    Nothing -> do
        response <- maybe (pure Nothing) (parse url) json
        pure $ Right response

sendMessage :: Integer -> Text -> Bot ()
sendMessage userId msg = defaultOpts >>= sendMessageInternal userId msg

sendMessageWithKeyboard :: Integer -> Text -> Keyboard -> Bot ()
sendMessageWithKeyboard userId msg keyboard = optsWithKeyboard >>= sendMessageInternal userId msg
    where
      optsWithKeyboard = defaultOpts <&> param "keyboard" .~ [encodeToText keyboard]

sendMessageInternal :: Integer -> Text -> Wreq.Options -> Bot ()
sendMessageInternal userId msg opts = do
  let url = Url.messagesSend
  result <- Wreq.getWith url $ opts
      & param "user_id" .~ [showT userId]
      & param "peer_id" .~ [showT userId]
      & param "message" .~ [truncateMessageIfNeeded msg]
  messageId <- maybe (pure Nothing) (parse url) result :: Bot (Maybe MessageId)
  case messageId of
    Just mId -> updateState mId
    Nothing  -> pure()
  where
    updateState :: MessageId -> Bot ()
    updateState messageId = do
      _ <- updateStateForUser userId (\st -> st & lastSentMessageId .~ getId messageId)
      pure ()

    messageLengthLimit = 3912 -- Found experimentally
    truncateMessageIfNeeded :: Text -> Text
    truncateMessageIfNeeded m 
      | T.length m <= messageLengthLimit = m
      | otherwise = 
        let message = "Макс. размер соообщения - " <> showT messageLengthLimit <> " символов. Сообщение будет обрезано.\n"
        in
          message <> (T.take (messageLengthLimit - T.length message) m)

-- Parses with outputting protocol errors or parsing errors to log
parse :: FromJSON a => Text -> LBS.ByteString -> Bot (Maybe a)
parse methodName bs = case eitherParse bs of
    Left e       -> logError e methodName >> pure Nothing
    Right result -> pure $ Just result

-- Parses without outputting anything
parseSilent :: FromJSON a => LBS.ByteString -> Bot (Maybe a)
parseSilent bs = pure $ rightToMaybe (eitherParse bs)

eitherParse :: FromJSON a => LBS.ByteString -> Either Error a
eitherParse bs =
    case decode bs :: Maybe Error of
      Just err  -> Left err
      Nothing ->
        case decode bs of
          Just r  -> Right r
          Nothing -> Left parsingError
    where
      parsingError = ParsingError "Failed to parse JSON" (TE.decodeUtf8 $ LBS.toStrict bs)

logError :: Error -> Text -> Bot ()
logError ParsingError{..} methodName =
  Log.error [F.text|'Request: ${methodName}'
    Failed to parse JSON:
    ${_json}|]
logError e@Error{..} methodName = do
  let source = Utils.prettifyError e
  Log.error [F.text|'Request ${methodName}'
    Received error:
    ${source}|]
