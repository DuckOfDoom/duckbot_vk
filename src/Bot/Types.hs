{-# LANGUAGE TemplateHaskell #-}

module Bot.Types
  ( Bot
  , Env(..)
  , config
  , logger
  , lastSentMessageId
  , quizState
  ) where

import Bot.Config
import BotPrelude
import Control.Lens (makeLenses)
import Data.Text    (Text)

import Modules.CofQuiz.Types (QuizState)

type Bot = ReaderT Env IO

data Env = Env
  { _config            :: Config
  , _logger            :: !(Text -> IO ())
  , _lastSentMessageId :: Integer
  , _quizState         :: QuizState
  }

makeLenses ''Env
