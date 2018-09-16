{-# LANGUAGE TemplateHaskell #-}

module Modules.Quiz.Types
  ( QuizState(..)
  , currentQuestion
  , score
  , defaultState
  )
  where

import BotPrelude

data QuizState = QuizState
  { _currentQuestion :: Maybe Text
  , _score           :: (Int, Int)
  } deriving (Show)

defaultState :: QuizState
defaultState = QuizState Nothing (0, 0)

makeLenses ''QuizState
