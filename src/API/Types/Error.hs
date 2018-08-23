{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Types.Error
  ( Error(..)
  , code
  , msg
  , requestParams
  , json
  , message
  )
  where

import BotPrelude

import qualified Data.HashMap.Strict  as HM

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