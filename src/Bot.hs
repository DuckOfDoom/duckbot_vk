module Bot
    (
    start
    ) where

import           Bot.Types            (Bot, config)
import           Control.Lens         ((^.))
import           Control.Monad.Reader (ask)
import           BotPrelude
import           Utils.Logging        (logInfo)

start :: Bot ()
start = do
  logInfo "Starting..."
  env <- ask
  logInfo $ showT $ env ^. config
