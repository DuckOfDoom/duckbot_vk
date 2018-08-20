{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  ( getLongPollServer
  , mkLongPollServerUrl
  ) where

import BotPrelude
import NeatInterpolation (text)

getLongPollServer :: Text
getLongPollServer = composeApi "messages.getLongPollServer"

mkLongPollServerUrl :: Text -> Text
mkLongPollServerUrl server = "https://" <> server

composeApi :: Text -> Text
composeApi method = [text|https://api.vk.com/method/${method}|]
