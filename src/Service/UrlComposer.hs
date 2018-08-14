{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  ( getLongPollServer
  ) where

import BotPrelude
import NeatInterpolation (text)

getLongPollServer :: Text
getLongPollServer = compose "messages.getLongPollServer"

type MethodName = Text

compose :: MethodName -> Text
compose method = [text|https://api.vk.com/method/${method}|]

