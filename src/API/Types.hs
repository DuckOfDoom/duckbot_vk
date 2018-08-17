{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Types
  ( Response(..)
  , prettifyError
  ) where

import BotPrelude

import           Data.Aeson.Encode.Pretty (Config(..), Indent(..),
                                           NumberFormat(..), encodePretty',
                                           keyOrder)
import           Data.Aeson.Types         (typeMismatch)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import           Data.HashMap.Strict      as HM (toList)
import           Data.Text.Encoding       as T

prettifyError :: Response -> Text
prettifyError e = T.decodeUtf8 $ LBS.toStrict $ encodePretty' conf e
  where
    conf = Config
      { confIndent = Spaces 2
      , confCompare = keyOrder ["code", "message", "requestParams"]
      , confNumFormat = Generic
      , confTrailingNewline = False
      }

data Response
 = None
 | Error
  { code          :: Int
  , message       :: Text
  , requestParams :: [HashMap Text Text]
  }
 | LongPollServerSettings
  { key    :: Text
  , server :: Text
  , ts     :: Text
  } deriving (Show, Eq, Generic)

makeLenses ''Response

instance FromJSON Response where
  parseJSON (Object o) =
    case head $ HM.toList o of
      Just ("error", Object v) -> Error
        <$> v .: "error_code"
        <*> v .: "error_msg"
        <*> v .: "request_params"
      _ -> typeMismatch "Response" (Object o)
  parseJSON invalid = typeMismatch "Response" invalid

instance ToJSON Response
