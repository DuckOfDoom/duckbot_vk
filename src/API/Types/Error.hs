{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Types.Error
  ( Error(..)
  , code
  , msg
  , requestParams
  , json
  , message
  , prettifyError
  )
  where

import BotPrelude

import qualified Data.ByteString.Lazy as LBS (toStrict)
import qualified Data.HashMap.Strict  as HM
import qualified Data.Text.Encoding   as E

import Data.Aeson.Encode.Pretty (Config(..), Indent(..), NumberFormat(..),
                                 encodePretty', keyOrder)

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
