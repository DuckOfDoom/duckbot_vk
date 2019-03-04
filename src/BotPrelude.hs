module BotPrelude
  ( module Protolude
  , module RE
  , module BotPrelude
  ) where

import Control.Lens as RE (makeClassy, makeFields, makeFieldsNoPrefix,
                           makeLenses, makePrisms, (%~), (.~), (?~), (^.),
                           (^..), (^?), (^?!), _1, _2, _3, _4, _Just, _Nothing)
import Data.Aeson   as RE (FromJSON, ToJSON, Value(..), object, parseJSON,
                           toJSON, withObject, withScientific, (.:), (.:?),
                           (.=))

-- import Data.Aeson.Encode.Pretty as RE (encodePretty)
-- import Data.Aeson.Types         as RE (Parser)
import Data.HashMap.Strict      as RE (HashMap)

import Control.Concurrent.MVar as RE ()

import Data.Aeson.Text (encodeToLazyText)

import Data.Time
import Protolude hiding (print)

import System.Random as RE (randomIO, randomRIO)

import qualified Data.Text      as T
import qualified Data.Text.Lazy as LT

getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

showT :: Show a => a -> Text
showT = T.pack . show

prnt :: (Show a) => a -> IO ()
prnt = putStrLn . showT

encodeToText :: (ToJSON a) => a -> Text
encodeToText = LT.toStrict . encodeToLazyText
