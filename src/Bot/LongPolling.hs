module Bot.LongPolling
  ( startLongPolling
  ) where

import API.Types (LongPollingServerSettings)
import API.Requests (getLongPollingServer)
import Bot.Types    (Bot)
import BotPrelude

import Service.Logging (logInfo)

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  initialSettings <- getLongPollingServer
  return () 
    where 
      mkRequest :: LongPollingServerSettings -> Bot ()
      mkRequest s = 
