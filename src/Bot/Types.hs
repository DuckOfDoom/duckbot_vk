{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Bot.Types
  ( Bot
  , Env(..)
  , config
  , logger
  ) where

import           Bot.Config
import           Control.Lens         (makeLenses)
import           Control.Monad.Reader
import           Data.Text            (Text)
import           BotPrelude

type Bot = ReaderT Env IO

data Env = Env
  { _config :: Config
  , _logger :: !(Text -> IO ())
  }

makeLenses ''Env

