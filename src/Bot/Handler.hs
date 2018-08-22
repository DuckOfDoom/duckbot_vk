module Bot.Handler
  ( handle
  )
  where

import API.Types.Update
import Bot.Types
import BotPrelude hiding (handle)
import Service.Logging

handle :: Update -> Bot ()
handle u = do
  logInfo $ "Handler. Received Update: " <> showT u
  pure ()

  