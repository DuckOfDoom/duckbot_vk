module Bot
  ( startBot
  ) where

import Bot.Config      (Config)
import Bot.LongPolling (startLongPolling)
import Bot.Server      (runServer)
import Bot.Types       (Env(..), defaultState)
import BotPrelude
import Data.Aeson      (decodeFileStrict)

import qualified Bot.Handler     as Handler (handle)
import qualified Service.Logging as Logging (processLog)

startBot :: IO ()
startBot = do
  env <- initEnv
  let
    startInNewThread f = forkIO $ do 
      _ <- runReaderT (runStateT f defaultState) env
      pure ()
    loop = threadDelay 1000 >> loop

  mapM_ startInNewThread
    [ runServer
    , startLongPolling Handler.handle
    ]

  loop

initEnv :: IO Env
initEnv = do
  config' <- readConfig
  pure $ Env
   { _config = config'
   , _logger = Logging.processLog
   }
   where
    readConfig :: IO Config
    readConfig = do
      c <- decodeFileStrict "config.json"
      case c of
        Nothing   -> die "Can't decode 'config.json'"
        Just conf -> pure conf