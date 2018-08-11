module Bot
  ( startBot
  ) where

import           Bot.Config      (Config)
import           Bot.LongPolling (startLongPolling)
import           Bot.Server      (runServer)
import           Bot.Types       (Env (..))
import           BotPrelude
import           Data.Aeson      (decodeFileStrict)

startBot :: IO ()
startBot = do
  env <- initEnv
  let
    startInNewThread f = forkIO $ runReaderT f env
    loop = threadDelay 1000 >> loop

  mapM_ startInNewThread
    [ runServer
    , startLongPolling
    ]

  loop

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
        Nothing   -> die "Can't decode 'config.json'"
        Just conf -> pure conf
