{-# LANGUAGE TemplateHaskell #-}

module Bot.Handler
  ( UpdateHandler
  , HandlerState(..)
  , handle
  )
  where

import API.Types.Update (Update(..))
import API.Types.Message (getId)
import qualified API.Requests as API (sendMessage)

import Bot.Types (Bot)
import BotPrelude hiding (handle)
import qualified Service.Logging as Log (info)

type UpdateHandler = Update -> StateT HandlerState Bot()

data HandlerState = HandlerState
  { _lastSentMessageId :: Integer
  }
  deriving (Show)

makeLenses ''HandlerState

handle :: UpdateHandler
handle m@Message{..} = do
  -- TODO: Rewrite this stuff using Prisms
  Log.info $ "Handler. Received Message: " <> showT m
  lastSent <- getLastSentId
  -- Ignore my own updates
  when (_messageId /= lastSent) $ do
    mId <- lift $ API.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
    putLastSentId (maybe 0 getId mId)
    where
      getLastSentId :: StateT HandlerState Bot Integer
      getLastSentId = get >>= (\st -> pure $ st ^. lastSentMessageId)

      putLastSentId :: Integer -> StateT HandlerState Bot ()
      putLastSentId id = get >>= (\st -> pure $ st & lastSentMessageId .~ id) >>= put 

handle Undefined = pure ()
  