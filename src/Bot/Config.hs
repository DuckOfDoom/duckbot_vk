{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
  ( Config
  , accessToken
  , apiVersion
  , longPollVersion
  , port
  , admins
  , riotConfig
  , RiotGamesAPIConfig
  , apiKey
  , accountId
  , summonerId
  ) where

import BotPrelude

data RiotGamesAPIConfig = RiotGamesAPIConfig
  { _apiKey     :: Text
  , _accountId  :: Integer
  , _summonerId :: Integer
  } deriving (Show, Generic)

makeLenses ''RiotGamesAPIConfig

instance FromJSON RiotGamesAPIConfig where
  parseJSON = withObject "Config" $ \v -> RiotGamesAPIConfig
     <$> v .:  "api_key"
     <*> v .:  "account_id"
     <*> v .:  "summoner_id"

data Config = Config
  { _accessToken     :: Text
  , _apiVersion      :: Text
  , _longPollVersion :: Text
  , _port            :: Int
  , _admins          :: [Integer]
  , _riotConfig      :: Maybe RiotGamesAPIConfig
  } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
     <$> v .:  "access_token"
     <*> v .:  "api_version"
     <*> v .:  "long_poll_version"
     <*> v .:  "port"
     <*> v .:  "admins"
     <*> v .:? "riot_config"
