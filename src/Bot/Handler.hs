module Bot.Handler
  ( handle
  )
  where

import VK.Types.Update (Update(..), fromUser, text)
import Bot.Types (Bot, lastSentMessageId)
import BotPrelude hiding (handle)

import qualified Service.Logging as Log (info)
-- import qualified VK.Requests as VK (sendMessage)
import qualified Modules.Quiz as Quiz 

handle :: Update -> Bot ()
handle m@Message{..} = do
  lastSent <- getLastSentId
  -- Ignore my own updates
  when (_messageId > lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
   -- TODO: Rewrite Updates without sum types so we don't need to use fromJust?
    Quiz.replyToMessage (m ^?! fromUser) (m ^?! text)
    -- VK.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
    pure () 
    where 
        getLastSentId = (^. lastSentMessageId) <$> get

handle Undefined = pure ()
  