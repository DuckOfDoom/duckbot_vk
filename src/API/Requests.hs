{-# LANGUAGE QuasiQuotes     #-}

module API.Requests
  ( getLongPollingServer
  , parseResponse
  , parse
  ) where

import BotPrelude

import API.Types            (Error(..), LongPollServerSettings(..), prettifyError)
import Bot.Config           (longPollVersion)
import Bot.Types            (Bot, config)
import Data.Aeson           (decode)
import Data.ByteString.Lazy as LBS
import Data.Text.Encoding   as T
import Network.Wreq         (param)
import Service.Logging      (logError, logInfo)
import Service.UrlComposer  as Url (getLongPollServer)
import Service.Wreq         (getWith)

import NeatInterpolation

logRequest :: Text -> Bot ()
logRequest r = logInfo [text|Making request '${r}'|]

getLongPollingServer :: Bot (Maybe (Text, Text, Text))
getLongPollingServer = do
  lp_version <- (^. (config . longPollVersion)) <$> ask
  settings <- getWith Url.getLongPollServer (patch lp_version) >>= maybe (pure Nothing) parseSettings
  logInfo $ "Got Result:" <> showT settings
  pure Nothing
    where
      patch ver o = o & param "lp_version" .~ [ver]

      parseSettings :: LBS.ByteString -> Bot (Maybe LongPollServerSettings)
      parseSettings = parse Url.getLongPollServer

parseResponse :: FromJSON a => LBS.ByteString -> Either Error a
parseResponse bs = 
    case (decode bs) :: Maybe Error of
      Just err  -> Left err
      Nothing -> 
        case decode bs of 
          Just r -> Right r 
          Nothing -> Left $ parsingError bs
    where
      parsingError bs = ParsingError
        { message = "Failed to parse JSON"
        , json = T.decodeUtf8 $ LBS.toStrict bs
        }

parse :: FromJSON a => Text -> LBS.ByteString -> Bot (Maybe a)
parse methodName bs = case parseResponse bs of 
    Left e@ParsingError{..} -> do
        logError [text|'Request: ${methodName}'
        Failed to parse JSON:
        ${json}|]
        pure Nothing
    Left e@Error{..} -> do
        let source = prettifyError e
        logError [text|'Request ${methodName}'
        Received error:
        ${source}|]
        pure Nothing
    Right result -> pure $ Just result