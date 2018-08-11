module Bot.LongPolling 
  ( startLongPolling
  ) where
  
import BotPrelude 
import Bot.Types (Bot) 

import Service.Logging (logInfo)

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  return ()
