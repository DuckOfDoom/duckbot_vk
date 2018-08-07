module Bot
  ( startBot
  ) where

import           Bot.Config    (Config)
import           Bot.Server    (runServer)
import           Bot.Types     (config)
import           Bot.Types     (Env (..))
import           BotPrelude
import           Data.Aeson    (decodeFileStrict)
import           Utils.Logging (logInfo)

startBot :: IO ()
startBot = do
  env <- initEnv
  runReaderT start env
    where
     start = do
      e <- ask
      logInfo $ "Config:\n " <> show (e ^. config)
      runServer

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

