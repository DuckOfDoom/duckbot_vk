{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module API.Types.LongPolling
  ( LongPollServerSettings(..)
  , server
  , key
  , LongPollResponse(..)
  , ts
  , updates
  )
where

import qualified Data.HashMap.Strict as HM
import qualified Data.Text           as T

import API.Types.Update
import BotPrelude
import GHC.Show         (Show(..))

-- every response is in nested "response" object
parseResponse :: MonadPlus m => (_ -> m a) -> Value -> m a
parseResponse responseParser (Object o) =
  case head $ HM.toList o of
    Just ("response", Object v) -> responseParser v
    _                           -> mzero
parseResponse _ _ = mzero

data LongPollServerSettings = LongPollServerSettings
  { _server :: Text
  , _key    :: Text
  , _ts     :: Integer
  }
  deriving (Eq, Generic)

makeFieldsNoPrefix ''LongPollServerSettings

instance FromJSON LongPollServerSettings where
  parseJSON = parseResponse $ \v ->
    LongPollServerSettings
        <$> v .: "server"
        <*> v .: "key"
        <*> v .: "ts"

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
