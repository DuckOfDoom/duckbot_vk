{-# LANGUAGE DeriveGeneric #-}

module API.Types.Message
  ( MessageId(..)
  )
  where

import qualified API.Types.Utils as Utils (parseResponse)
import           BotPrelude

newtype MessageId = MessageId { getId :: Integer}
  deriving (Show, Eq, Generic)

instance FromJSON MessageId where
  parseJSON = Utils.parseResponse parseMessageId
    where 
      parseMessageId :: Value -> Parser MessageId
      parseMessageId v = do
        mId <- parseJSON v 
        pure $ MessageId mId
