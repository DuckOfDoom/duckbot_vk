{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer
  (

  ) where

import           BotPrelude
import           NeatInterpolation (text)

longPollingUrl :: Text
longPollingUrl = compose "messages.getLongPollServer"

type MethodName = Text

compose :: MethodName -> Text
compose method =
-- Parameters are added via wreq
-- [text|https://api.vk.com/method/${method}?${parameters}&access_token=${token}&v=${version}|]
  [text|https://api.vk.com/method/${method}|]

