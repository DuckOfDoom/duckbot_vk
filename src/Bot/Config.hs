{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
  ( Config
  , accessToken
  , apiVersion
  , longPollVersion
  , port
  , confirmationString
  ) where

import BotPrelude

data Config = Config
  { _accessToken        :: Text
  , _apiVersion         :: Double
  , _longPollVersion    :: Double
  , _port               :: Int
  , _confirmationString :: Maybe Text
  } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
     <$> v .:  "access_token"
     <*> v .:  "api_version"
     <*> v .:  "long_poll_version"
     <*> v .:  "port"
     <*> v .:? "confirmation"
