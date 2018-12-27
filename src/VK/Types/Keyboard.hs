{-# LANGUAGE DeriveGeneric #-}

module VK.Types.Keyboard
  ( Keyboard(..)
  , Color(..)
  , Action(..)
  , Button(..)
  )
  where

import BotPrelude
import Data.Aeson (Value(String))
import Data.Text  as T

data Color = Primary | Default | Negative | Positive
  deriving (Show)

instance ToJSON Color where
  toJSON c = String $ (T.toLower . showT) c

data Action = Action
  { type' :: Text
  , label :: Text
  } deriving (Show, Generic)

instance ToJSON Action where
  toJSON (Action t l) = object ["type" .= t, "label" .= l ]

data Button = Button
  { action :: Action
  , color  :: Color
  } deriving (Show, Generic)

instance ToJSON Button

data Keyboard = Keyboard
  { buttons  :: [[Button]]
  , one_time :: Bool
  } deriving (Show, Generic)

instance ToJSON Keyboard
