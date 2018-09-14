{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Types
  ( Bot
  , liftBot
  , Env(..)
  , config
  , logger
  , BotState (..)
  , defaultState
  , lastSentMessageId
  , quizState
  ) where

import Bot.Config
import BotPrelude
import Control.Lens (makeLenses)
import Data.Text    (Text)

import Modules.Quiz.Types (QuizState(..))

data BotState = BotState
  { _lastSentMessageId :: Integer
  , _quizState         :: QuizState
  }

makeLenses ''BotState

defaultState :: BotState
defaultState = BotState 
  { _lastSentMessageId = 0
  , _quizState = QuizState
    { _currentQuestion = Nothing
    }
  }

type Bot = StateT BotState (ReaderT Env IO)

liftBot :: IO a -> Bot a
liftBot = lift . lift

data Env = Env
  { _config :: Config
  , _logger :: !(Text -> IO ())
  }

makeLenses ''Env
