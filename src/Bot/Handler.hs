module Bot.Handler
  ( handle
  )
  where

import API.Types.Update (Update(..))
import API.Types.Message (getId)
import qualified API.Requests as API (sendMessage)

import Bot.Types (Bot)
import BotPrelude hiding (handle)
import Service.Logging (logInfo)

handle :: Update -> StateT Integer Bot ()
handle m@Message{..} = do
  logInfo $ "Handler. Received Message: " <> showT m
  lastSentMessage <- get 
  -- Ignore my own updates
  when (_messageId /= lastSentMessage) $ do
    mId <- lift $ API.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
    put $ maybe 0 getId mId
handle Undefined = pure ()
  