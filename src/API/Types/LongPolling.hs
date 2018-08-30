{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module API.Types.LongPolling
  ( LongPollServerSettings(..)
  , server
  , key
  , LongPollResponse(..)
  , ts
  , updates
  , Failed(..)
  )
where

import           API.Types.Update (Update)
import qualified API.Types.Utils  as Utils (parseNested)

import BotPrelude
import GHC.Show   (Show(..))

data LongPollServerSettings = LongPollServerSettings
  { _server :: Text
  , _key    :: Text
  , _ts     :: Integer
  }
  deriving (Show, Eq, Generic)

makeFieldsNoPrefix ''LongPollServerSettings

instance FromJSON LongPollServerSettings where
  parseJSON = Utils.parseNested "response" parseSettings
    where
      parseSettings (Object o) =
        LongPollServerSettings
            <$> o .: "server"
            <*> o .: "key"
            <*> o .: "ts"
      parseSettings _ = mzero

instance ToJSON LongPollServerSettings

data LongPollResponse = LongPollResponse
  { _ts      :: Integer
  , _updates :: [Update]
  }
  deriving (Show, Eq, Generic)

makeFieldsNoPrefix ''LongPollResponse

instance FromJSON LongPollResponse where
  parseJSON (Object v) = LongPollResponse
    <$> v .: "ts"
    <*> v .: "updates"
  parseJSON _ = mzero

data Failed = Failed
 { _failCode :: Integer
 , _ts       :: Maybe Integer
 }

instance FromJSON Failed where
  parseJSON (Object v) = Failed
    <$> v .: "failed"
    <*> v .:? "ts"
  parseJSON _ = mzero