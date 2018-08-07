module Bot
  ( startBot
  ) where

import           Bot.Types            (Bot, config)
import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask)
import           BotPrelude
import           Utils.Logging        (logInfo)
import           Control.Monad.Reader
import           Data.Text            as T

import           Bot.Config           (Config)
import           Bot.Types            (Env (..))

import           Data.Aeson           (decodeFileStrict)

startBot :: IO ()
startBot = do
  env <- initEnv
  runReaderT start env
    where
     start = do
      logInfo "Starting..."
      e <- ask
      logInfo $ showT $ e ^. config

initEnv :: IO Env
initEnv = do
  config' <- readConfig
  pure $ Env
   { _config = config'
   , _logger = putStrLn . T.unpack
   }

readConfig :: IO Config
readConfig = do
  c <- decodeFileStrict "config.json"
  case c of
    Nothing   -> die $ "Can't find config file config.json"
    Just conf -> pure conf