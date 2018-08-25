{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
  ( Config
  , accessToken
  , apiVersion
  , longPollVersion
  , port
  , creatorId
  , confirmationString
  ) where

import BotPrelude

data Config = Config
  { _accessToken        :: Text
  , _apiVersion         :: Text
  , _longPollVersion    :: Text
  , _port               :: Int
  , _creatorId          :: Maybe Integer
  , _confirmationString :: Maybe Text
  } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
     <$> v .:  "access_token"
     <*> v .:  "api_version"
     <*> v .:  "long_poll_version"
     <*> v .:  "port"
     <*> v .:  "creator_id"
     <*> v .:? "confirmation"
