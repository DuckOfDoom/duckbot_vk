{-# LANGUAGE QuasiQuotes     #-}

module API.Requests
  ( getLongPollingServer
  ) where

import BotPrelude

import API.Types            (Error(..), LongPollServerSettings(..),
                             prettifyError)
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
  logRequest $ "getLongPollingServer"
  lp_version <- (^. (config . longPollVersion)) <$> ask
  settings <- getWith Url.getLongPollServer (patch lp_version) >>= maybe (pure Nothing) parseSettings
  logInfo $ "Got Result:" <> showT settings
  pure Nothing
    where
      patch ver o = o & param "lp_version" .~ [ver]
      parseSettings :: LBS.ByteString -> Bot (Maybe LongPollServerSettings)
      parseSettings = parse

parse :: FromJSON a => LBS.ByteString -> Bot (Maybe a)
parse bs = do 
    e <- parseError bs
    case e of 
      Nothing -> parseResponse bs
      Just _  -> pure  Nothing
    where
      parseError :: LBS.ByteString -> Bot (Maybe Error)
      parseError s =
        case (decode s) of
          Nothing -> pure Nothing
          Just e -> do
            let source = prettifyError e
            logError  [text|Got error:
            ${source}|]
            pure $ Just e

      parseResponse :: FromJSON a => LBS.ByteString -> Bot (Maybe a)
      parseResponse s =
        case (decode s) of
          Nothing -> do
            let source = T.decodeUtf8 $ LBS.toStrict bs
            logError [text|Failed to parse JSON:
            ${source}|]
            pure $ Nothing

          Just result ->
            pure $ result