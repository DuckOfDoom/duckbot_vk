module Service.UrlComposer
  ( messagesGetLongPollServer
  , messagesSend
  , mkLongPollServerUrl
  ) where

import BotPrelude
import qualified NeatInterpolation as F

messagesGetLongPollServer :: Text
messagesGetLongPollServer = composeApi "messages.getLongPollServer"

messagesSend :: Text
messagesSend = composeApi "messages.send"

mkLongPollServerUrl :: Text -> Text
mkLongPollServerUrl server = "https://" <> server

composeApi :: Text -> Text
composeApi method = [F.text|https://api.vk.com/method/${method}|]
