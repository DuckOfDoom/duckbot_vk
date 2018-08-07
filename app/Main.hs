module Main where

import           Control.Monad.Reader
import           Data.Text            as T

import           Bot                  (start)
import           Bot.Config           (Config)
import           Bot.Types            (Env (..))

import           Data.Aeson           (decodeFileStrict)

import           BotPrelude

main :: IO ()
main = do
  initEnv >>= runReaderT start

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
