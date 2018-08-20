module Bot.LongPolling
  ( startLongPolling
  ) where

import API.Requests    (getLongPollingServer, longPoll)
import API.Types       (LongPollServerSettings)
import Bot.Types       (Bot)
import BotPrelude
import Service.Logging (logInfo)

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  initialSettings <- getLongPollingServer
  logInfo $ "Got initial settings:\n" <> showT initialSettings
  maybe (pure ()) mkRequest initialSettings
    where
      mkRequest :: LongPollServerSettings -> Bot ()
      mkRequest s = do
        res <- longPoll s
        pure ()

