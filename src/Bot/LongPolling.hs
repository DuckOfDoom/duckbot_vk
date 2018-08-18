module Bot.LongPolling
  ( startLongPolling
  ) where

import API.Requests (getLongPollingServer)
import Bot.Types    (Bot)
import BotPrelude

import Service.Logging (logInfo)

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  _ <- getLongPollingServer
  return ()

