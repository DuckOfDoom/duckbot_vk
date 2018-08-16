{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}

module API.Types
  ( Response(..)
  ) where

import BotPrelude

import Data.Aeson.Types    (typeMismatch)
import Data.HashMap.Strict as HM (toList)

-- instance FromJSON (Text, Text) where
--   parseJSON (Object o) = 
--       let m = (parseJSON o) :: Parser 

data Response
 = None
 | Error
  { code          :: Int
  , message       :: Text
  , requestParams :: [(Text, Text)]
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
