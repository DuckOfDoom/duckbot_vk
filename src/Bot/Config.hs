{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Config
  ( Config
  , token
  )
where

import           Data.Text (Text)
import           BotPrelude

data Config = Config
  { _token :: Text
  } deriving (Show, Generic)

makeLenses ''Config

instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
     <$> v .: "token"
