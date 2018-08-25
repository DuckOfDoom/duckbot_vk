{-# LANGUAGE RankNTypes #-}

module Service.Logging
  ( info
  , error
  , HasLog
  ) where

import Bot.Types            (Env, logger)
import BotPrelude           hiding (log)
import Control.Lens         ((^.))
import Control.Monad.Reader (MonadIO, MonadReader, ask, liftIO)

class HasLog a where
  getLog :: a -> (Text -> IO ())

instance HasLog Env where
  getLog e = e ^. logger

-- TODO: Make func work with (Show a)?
type LogFunc env m = (MonadReader env m, HasLog env, MonadIO m)
             => Text
             -> m ()
info :: LogFunc env m
info = log "INFO:"

error :: LogFunc env m
error = log "ERROR:"
log :: (MonadReader env m, HasLog env, MonadIO m)
             => Text -- prefix
             -> Text -- message
             -> m ()
log prefix msg = do
  env <- ask
  -- TODO: Add time to logs?
  --t <- fmap formatTime getCurrentTime
  liftIO $ getLog env $ mconcat $ intersperse " "
    [ prefix
    , msg
    ]
--  where
--    formatTime :: UTCTime -> Text
--    formatTime = showT
