{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE DeriveGeneric          #-}

module API.Types.Update 
  ( Update(..)
  , fromUser
  , text
  , timestamp
  )
where

import BotPrelude

data Update
   = Undefined
   | Message
   { _fromUser  :: Integer
   , _timestamp :: Integer
   , _text      :: Text
   }
  deriving (Show, Eq, Generic)

makeLenses ''Update

instance FromJSON Update where
  parseJSON (Array a) = do
    uType <- (parseUpdateType $ head a)
    case (uType :: Integer) of
      -- Message goes as follows [0:updateType, 1:message_id, 2:flags, 3:peer_id, 4:timestamp, 5: text ]
      4 -> parseMessage $ (drop 3 . toList) a
      _ -> pure Undefined
    where
      parseUpdateType (Just v) = parseJSON v
      parseUpdateType _        = mzero

      parseMessage (peerId:ts:messageText:_) = Message <$> parseJSON peerId <*> parseJSON ts <*> parseJSON messageText
      parseMessage _        = mzero

  parseJSON _ = mzero