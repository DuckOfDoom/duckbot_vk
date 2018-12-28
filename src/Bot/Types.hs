{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}

module Bot.Types
  ( Bot
  , liftBot
  , Env(..)
  , config
  , logger
  , BotState (..)
  , updateStateForUser
  , getStateForUser
  , lastSentMessageId
  , quizState
  ) where

import Bot.Config
import BotPrelude
import Control.Lens (makeLenses)
import Data.Text    (Text)
import Data.HashMap.Strict (HashMap)

import qualified Data.HashMap.Strict as HM (lookupDefault, insert)

import qualified Modules.Quiz.Types as Quiz (QuizState(..), defaultState)

data BotState = BotState
  { _lastSentMessageId :: Integer
  , _quizState         :: Quiz.QuizState
  } deriving (Show)

makeLenses ''BotState

defaultState :: BotState
defaultState = BotState
  { _lastSentMessageId = 0
  , _quizState = Quiz.defaultState
  } 

type Bot = StateT (HashMap Integer BotState) (ReaderT Env IO)

updateStateForUser :: Integer -> (BotState -> BotState) -> Bot BotState
updateStateForUser userId update = do
  userState <- getStateForUser userId
  let newState = update userState
  HM.insert userId newState <$> get >>= put
  pure newState

getStateForUser :: Integer -> Bot BotState
getStateForUser userId = HM.lookupDefault defaultState userId <$> get

liftBot :: IO a -> Bot a
liftBot = lift . lift

data Env = Env
  { _config :: Config
  , _logger :: !(Text -> IO ())
  }

makeLenses ''Env