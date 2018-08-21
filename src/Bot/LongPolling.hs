module Bot.LongPolling
  ( startLongPolling
  ) where

import API.Requests    (getLongPollingServer, longPoll)
import API.Types       (LongPollResponse(..), LongPollServerSettings(..),
                        initialTs, key, server, ts)
import Bot.Types       (Bot)
import BotPrelude
import Service.Logging (logError, logInfo)

startLongPolling :: Bot ()
startLongPolling = do
  logInfo "Starting long polling..."
  initialSettings <- getLongPollingServer
  logInfo $ "Got initial settings:\n" <> showT initialSettings
  maybe (pure ()) mkRequest initialSettings
    where
      mkRequest :: LongPollServerSettings -> Bot ()
      mkRequest s = do
        res <- longPoll s
        case res of
          Nothing -> do
            logError "Didn't get any result on long polling! Retrying in 5 seconds..."
            liftIO $ threadDelay 5000000
            mkRequest s
          Just r -> mkRequest (s & initialTs .~ (r ^. ts))
