{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
  ( Config
  , token
  , host
  , port
  )
where

import           BotPrelude

data Config = Config
  { _token :: Text
  , _host  :: Text
  , _port  :: Int
  } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
     <$> v .: "token"
     <*> v .: "host"
     <*> v .: "port"
