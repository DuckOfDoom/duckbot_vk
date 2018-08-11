module BotPrelude
  ( module Protolude
  , module RE
  , module BotPrelude
  ) where

import           Control.Lens as RE (makeLenses, makePrisms, (%~), (.~), (?~),
                                     (^.), (^..), (^?), _1, _2, _3, _4, _Just,
                                     _Nothing)
import           Data.Aeson   as RE (FromJSON, ToJSON, object, parseJSON,
                                     toJSON, withObject, (.:), (.:?), (.=))
import           Data.Time
import           Protolude

import qualified Data.Text    as T

--import Data.List as RE
--  (lookup, nub)
--import Data.Time as RE
--  (UTCTime(..), Day(..))
--import System.Directory as RE
--  (doesFileExist)
--import System.Random as RE
--  (randomIO, randomRIO)


getCurrentTime :: MonadIO m => m UTCTime
getCurrentTime = liftIO Data.Time.getCurrentTime

showT :: Show a => a -> Text
showT = T.pack . show
