{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE RecordWildCards       #-}
{-# LANGUAGE TemplateHaskell       #-}

{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module API.Types
  ( Error(..)
  , LongPollServerSettings(..)
  , prettifyError
  ) where

import BotPrelude

import           Data.Aeson.Encode.Pretty (Config(..), Indent(..),
                                           NumberFormat(..), encodePretty',
                                           keyOrder)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import qualified Data.HashMap.Strict      as HM
import           Data.Text.Encoding       as T

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
  { error_code     :: Int
  , error_msg      :: Text
  , request_params :: [HashMap Text Text]
  }
  | ParsingError
  { message :: Text
  , json    :: Text
  }
   deriving (Show, Eq, Generic)

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
makeLenses ''Error

prettifyError :: Error -> Text
prettifyError e = T.decodeUtf8 $ LBS.toStrict $ (encodePretty' conf e)
  where
    conf = Config
      { confIndent = Spaces 2
      , confCompare = keyOrder ["error_msg", "error_code", "request_params", "key", "value"]
      , confNumFormat = Generic
      , confTrailingNewline = False
      }

-- LongPollServerSettings ----------------------------------------------
data LongPollServerSettings = LongPollServerSettings
  { key    :: Text
  , server :: Text
  , ts     :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON LongPollServerSettings where
  parseJSON = parseResponse $ \v ->
    LongPollServerSettings
        <$> v .: "key"
        <*> v .: "server"
        <*> v .: "ts"

instance ToJSON LongPollServerSettings

-----------------------------------------------------------------------
