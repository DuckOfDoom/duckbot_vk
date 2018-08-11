module Bot
  ( startBot
  ) where

import           Bot.Config         (Config)
import           Bot.LongPolling    (startLongPolling)
import           Bot.Server         (runServer)
import           Bot.Types          (config,Bot)
import           Bot.Types          (Env (..))
import           BotPrelude
import           Control.Concurrent (forkIO)
import           Data.Aeson         (decodeFileStrict)
import           Service.Logging    (logInfo)

startBot :: IO ()
startBot = do
  env <- initEnv
  runReaderT start env
    where
     start :: ReaderT Env IO ()
     start = do
      e <- ask
      logInfo $ "Config:\n " <> show (e ^. config)
      startLongPolling
      runServer
      pure ()
      
initEnv :: IO Env
initEnv = do
  config' <- readConfig
  pure $ Env
   { _config = config'
   , _logger = putStrLn
   }
   where
    readConfig :: IO Config
    readConfig = do
      c <- decodeFileStrict "config.json"
      case c of
        Nothing   -> die $ "Can't decode 'config.json'"
        Just conf -> pure conf
