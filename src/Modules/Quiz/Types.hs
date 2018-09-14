{-# LANGUAGE TemplateHaskell #-}

module Modules.Quiz.Types
  ( QuizState(..) 
  , currentQuestion
  , defaultState
  )
  where

import BotPrelude

data QuizState = QuizState
  { _currentQuestion :: Maybe Text 
  }

defaultState :: QuizState
defaultState = QuizState Nothing

makeLenses ''QuizState