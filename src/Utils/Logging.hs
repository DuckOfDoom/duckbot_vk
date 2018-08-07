module Utils.Logging
 ( logInfo
 )where

import           Bot.Types            (Env, logger)
import           Control.Lens         ((^.))
import           Control.Monad.Reader (MonadIO, MonadReader, ask, liftIO)
import           Data.Monoid          ((<>))
import           Data.Text
import           BotPrelude

class HasLog a where
  getLog :: a -> (Text -> IO ())

instance HasLog Env where
  getLog e = e ^. logger

logInfo :: (MonadReader env m, HasLog env, MonadIO m)
             => Text
             -> m ()
logInfo msg = do
  env <- ask
  liftIO $ getLog env $ ("INFO:" <> msg)
