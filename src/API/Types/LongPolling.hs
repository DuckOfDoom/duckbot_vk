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
  )
where

import qualified Data.Text           as T

import API.Types.Update (Update)
import qualified API.Types.Utils as Utils (parseResponse)

import BotPrelude
import GHC.Show         (Show(..))

data LongPollServerSettings = LongPollServerSettings
  { _server :: Text
  , _key    :: Text
  , _ts     :: Integer
  }
  deriving (Eq, Generic)

makeFieldsNoPrefix ''LongPollServerSettings

instance FromJSON LongPollServerSettings where
  parseJSON = Utils.parseResponse parseSettings
    where
      parseSettings (Object o) = 
        LongPollServerSettings
            <$> o .: "server"
            <*> o .: "key"
            <*> o .: "ts"
      parseSettings _ = mzero

instance ToJSON LongPollServerSettings

instance Show LongPollServerSettings where
  show lps = T.unpack $ mconcat
   [ "Server: '" , lps ^. server
   , "'\nKey: '" , lps ^. key
   , "'\nTs: '"  , showT $ lps ^. ts
   , "'"
   ]

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
