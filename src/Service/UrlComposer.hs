{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  ( getLongPollServer
  ) where

import BotPrelude
import NeatInterpolation (text)

getLongPollServer :: Text
getLongPollServer = composeApi "messages.getLongPollServer"

-- composeLongPollingServer :: Text -> Text
-- composeLongPollingServer server = [text|${server}|]

composeApi :: Text -> Text
composeApi method = [text|https://api.vk.com/method/${method}|]
