{-# LANGUAGE TemplateHaskell #-}

module Bot.Types
  ( Bot
  , Env(..)
  , config
  , logger
  ) where

import           Bot.Config
import           BotPrelude
import           Control.Lens         (makeLenses)
import           Data.Text            (Text)

type Bot = ReaderT Env IO

data Env = Env
  { _config :: Config
  , _logger :: !(Text -> IO ())
  }

makeLenses ''Env

