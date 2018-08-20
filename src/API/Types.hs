{-# LANGUAGE DeriveGeneric          #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE QuasiQuotes            #-}
{-# LANGUAGE RecordWildCards        #-}
{-# LANGUAGE TemplateHaskell        #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module API.Types
  ( prettifyError
  , Error(..)
  , code
  , msg
  , requestParams
  , json
  , message
  , LongPollServerSettings(..)
  , server
  , key
  , initialTs
  , LongPollResponse(..)
  , ts
  -- , updates
  , Update(..)
  ) where

import BotPrelude

import           Data.Aeson.Encode.Pretty (Config(..), Indent(..),
                                           NumberFormat(..), encodePretty',
                                           keyOrder)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text                as T
import           Data.Text.Encoding       as E

import GHC.Show (Show(..))


-- every response is in nested "response" object

parseResponse :: MonadPlus m => (_ -> m a) -> Value -> m a
parseResponse responseParser (Object o) =
  case head $ HM.toList o of
    Just ("response", Object v) -> responseParser v
    _                           -> mzero
parseResponse _ _ = mzero

-- Error ----------------------------------------------

data Error
  = Error
  { _code          :: Int
  , _msg           :: Text
  , _requestParams :: [HashMap Text Text]
  }
  | ParsingError
  { _message :: Text
  , _json    :: Text
  }
   deriving (Show, Eq, Generic)
makeLenses ''Error

instance FromJSON Error where
  parseJSON (Object o) =
    case head $ HM.toList o of
      Just ("error", Object v) -> Error
        <$> v .: "error_code"
        <*> v .: "error_msg"
        <*> v .: "request_params"
      _ -> mzero
  parseJSON _ = mzero

instance ToJSON Error where

prettifyError :: Error -> Text
prettifyError e = E.decodeUtf8 $ LBS.toStrict $ encodePretty' conf e
  where
    conf = Config
      { confIndent = Spaces 2
      , confCompare = keyOrder ["_error_msg", "_error_code", "_request_params", "key", "value"]
      , confNumFormat = Generic
      , confTrailingNewline = False
      }

-- LongPollServerSettings ----------------------------------------------

data LongPollServerSettings = LongPollServerSettings
  { _server :: Text
  , _key    :: Text
  , _initialTs     :: Integer
  }
  deriving (Eq, Generic)

makeLenses ''LongPollServerSettings

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
   , "'\nTs: '"  , showT $ lps ^. initialTs
   , "'"
   ]

-----------------------------------------------------------------------
data Update = Update
  deriving (Show, Eq, Generic)

instance FromJSON Update 

data LongPollResponse = LongPollResponse
  { _ts      :: Integer
  -- , _updates :: Maybe [Update]
  }
  deriving (Show, Eq, Generic)

makeLenses ''LongPollResponse

instance FromJSON LongPollResponse where
  parseJSON (Object v) = LongPollResponse
    <$> v .: "ts"
    -- <*> v .: "updates"
  parseJSON _ = mzero