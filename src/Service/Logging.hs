{-# LANGUAGE RankNTypes #-}

module Service.Logging
  ( initLogging
  , info
  , error
  -- , test
  ) where

import           Bot.Types             (Env, logger)
import           BotPrelude            hiding (log)
import           Control.Lens          ((^.))
import           Control.Monad.Reader  (MonadIO, MonadReader, ask, liftIO)
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Format      (defaultTimeLocale, formatTime)
import qualified Utils

import           System.Log.FastLogger      (LogType(LogFileNoRotate, LogStdout),
                                             TimedFastLogger)
import qualified System.Log.FastLogger      as FastLogger (defaultBufSize,
                                                           newTimedFastLogger,
                                                           toLogStr)

logFileName :: FilePath
logFileName = "log.txt"

-- test :: IO ()
-- test = do
--   now <- getCurrentTime
--   putStrLn $ formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S.%s" now

initLogging :: IO (Text -> IO ())
initLogging = do
  -- A console logger
  (console, _) <- FastLogger.newTimedFastLogger getFormattedTime (LogStdout FastLogger.defaultBufSize)
  -- A file logger
  (file, _) <- FastLogger.newTimedFastLogger getFormattedTime (LogFileNoRotate logFileName FastLogger.defaultBufSize)
  pure $ logWithLoggers [console, file]
    where
      -- TODO: Add milliseconds to the time, this is very weird
      getFormattedTime = BS.pack . formatTime defaultTimeLocale "%Y-%m-%d %H:%M:%S" <$> getCurrentTime
      appendTimeToMessage message time = FastLogger.toLogStr (mconcat [decodeUtf8 time, " ", message, "\n"])

      logWithLoggers :: [TimedFastLogger] -> (Text -> IO ())
      logWithLoggers loggers msg = mapM_ ($ appendTimeToMessage msg) loggers

class HasLogging a where
  getLog :: a -> (Text -> IO ())

instance HasLogging Env where
  getLog e = e ^. logger

-- TODO: Make func work with (Show a)?
type LogFunc env m = (MonadReader env m, HasLogging env, MonadIO m)
             => Text
             -> m ()
info :: LogFunc env m
info = log "INFO:"

error :: LogFunc env m
error = log "ERROR:"
log :: (MonadReader env m, HasLogging env, MonadIO m)
             => Text -- prefix
             -> Text -- message
             -> m ()
log prefix msg = do
  env <- ask
  liftIO $ getLog env $ Utils.joinText " "
    [ prefix
    , msg
    ]
