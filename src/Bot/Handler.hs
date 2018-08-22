module Bot.Handler
  ( handle
  )
  where

import API.Types.Update (Update(..), fromUser, text)
import qualified API.Requests as API (sendMessage)
import Bot.Types
import BotPrelude hiding (handle)
import Service.Logging

handle :: Update -> Bot ()
handle m@Message{..} = do
  logInfo $ "Handler. Received Update: " <> showT m
  API.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
  pure ()
handle Undefined = pure ()

  