{-# LANGUAGE DeriveGeneric #-}

module VK.Types.Message
  ( MessageId(..)
  )
  where


import BotPrelude
import Data.Aeson.Types (Parser)

import qualified VK.Types.Utils as Utils (parseNested)

newtype MessageId = MessageId { getId :: Integer}
  deriving (Show, Eq, Generic)

instance FromJSON MessageId where
  parseJSON = Utils.parseNested "response" parseMessageId
    where
      parseMessageId :: Value -> Parser MessageId
      parseMessageId v = do
        mId <- parseJSON v
        pure $ MessageId mId
