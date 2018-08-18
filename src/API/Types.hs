{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}

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
import           Data.Text.Encoding       as T

data Error = Error
  { error_code     :: Int
  , error_msg      :: Text
  , request_params :: [HashMap Text Text]
  } deriving (Show, Eq, Generic)

instance FromJSON Error where
instance ToJSON Error where
makeLenses ''Error

prettifyError ::  Error -> Text
prettifyError e = T.decodeUtf8 $ LBS.toStrict $ (encodePretty' conf e)
  where
    conf = Config
      { confIndent = Spaces 2
      , confCompare = keyOrder ["code", "message", "requestParams"]
      , confNumFormat = Generic
      , confTrailingNewline = False
      }
-----------------------------------------------------------------------
data LongPollServerSettings = LongPollServerSettings
  { key    :: Text
  , server :: Text
  , ts     :: Text
  }
  deriving (Show, Eq, Generic)

instance FromJSON LongPollServerSettings
instance ToJSON LongPollServerSettings
-----------------------------------------------------------------------