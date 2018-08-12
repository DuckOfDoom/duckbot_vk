module Bot.LongPolling 
  ( startLongPolling
  ) where
  
import BotPrelude 
import Bot.Types (Bot) 

import Service.Logging (logInfo)

type ServerAddress = Text
type SecretKey = Text
type Timestamp = Text

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  return ()

