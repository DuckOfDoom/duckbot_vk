{-# LANGUAGE QuasiQuotes #-}

module Service.UrlComposer 
  (

  ) where

import NeatInterpolation (text)
import BotPrelude
  
longPollingUrl :: Text
longPollingUrl = do
  compose "messages.getLongPollServer"
  
type MethodName = Text

compose :: MethodName -> Text
compose method = 
-- Parameters are added via wreq
-- [text|https://api.vk.com/method/${method}?${parameters}&access_token=${token}&v=${version}|]
  [text|https://api.vk.com/method/${method}|]
  
