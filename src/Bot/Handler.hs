{-# LANGUAGE TemplateHaskell #-}

module Bot.Handler
  ( UpdateHandler
  , HandlerState(..)
  , handle
  )
  where

import VK.Types.Update (Update(..), fromUser, text)
import VK.Types.Message (getId)
import qualified VK.Requests as VK (sendMessage)

import Bot.Types (Bot)
import BotPrelude hiding (handle)
import qualified Service.Logging as Log (info)

import qualified Modules.CofQuiz as Quiz 

type UpdateHandler = Update -> StateT HandlerState Bot()

-- TODO: Put lastSentMessageid in Env. 
-- We can send multiple messages from modules.
-- Each VK.sendMessage  call should update env variable
data HandlerState = HandlerState
  { _lastSentMessageId :: Integer
  }
  deriving (Show)

makeLenses ''HandlerState

handle :: UpdateHandler
handle m@Message{..} = do
  lastSent <- getLastSentId
  -- Ignore my own updates
  when (_messageId /= lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
   -- TODO: Rewrite Updates without sum types so we don't need to use fromJust?
    lift $ Quiz.replyToMessage (m ^?! fromUser) (m ^?! text)
    mId <- lift $ VK.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
    putLastSentId (maybe 0 getId mId)
    where
      getLastSentId :: StateT HandlerState Bot Integer
      getLastSentId = get >>= (\st -> pure $ st ^. lastSentMessageId)

      putLastSentId :: Integer -> StateT HandlerState Bot ()
      putLastSentId id = get >>= (\st -> pure $ st & lastSentMessageId .~ id) >>= put 

handle Undefined = pure ()
  