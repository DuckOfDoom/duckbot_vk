module Bot.LongPolling
  ( startLongPolling
  ) where

import           API.Requests    (getLongPollingServer)
import           Bot.Types       (Bot)
import           BotPrelude

import           Service.Logging (logInfo)

type ServerAddress = Text
type SecretKey = Text
type Timestamp = Text

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  res <- getLongPollingServer
  return ()

