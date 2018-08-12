{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  ( getLongPoll
  ) where

import           BotPrelude
import           NeatInterpolation (text)

getLongPoll :: Text
getLongPoll = compose "messages.getLongPollServer"

type MethodName = Text

compose :: MethodName -> Text
compose method =
-- Parameters are added via wreq
-- [text|https://api.vk.com/method/${method}?${parameters}&access_token=${token}&v=${version}|]
  [text|https://api.vk.com/method/${method}|]

