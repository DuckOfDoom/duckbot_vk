module Bot.Handler
  ( handle
  )
  where

import API.Types.Update (Update(..))
import API.Types.Message (getId)
import qualified API.Requests as API (sendMessage)
import Bot.Types
import BotPrelude hiding (handle)
import Service.Logging

handle :: Update -> Bot ()
handle m@Message{..} = do
  logInfo $ "Handler. Received Update: " <> showT m
  mId <- API.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
  logInfo $ "Handler. Sent message: " <> showT mId
  pure ()
handle Undefined = pure ()

  