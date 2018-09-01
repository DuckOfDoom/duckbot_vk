{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  ( messagesGetLongPollServer
  , messagesSend
  , mkLongPollServerUrl
  ) where

import BotPrelude
import NeatInterpolation (text)

messagesGetLongPollServer :: Text
messagesGetLongPollServer = composeApi "messages.getLongPollServer"

messagesSend :: Text
messagesSend = composeApi "messages.send"

mkLongPollServerUrl :: Text -> Text
mkLongPollServerUrl server = "https://" <> server

composeApi :: Text -> Text
composeApi method = [text|https://VK.vk.com/method/${method}|]
