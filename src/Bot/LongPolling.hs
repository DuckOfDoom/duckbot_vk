module Bot.LongPolling
  ( startLongPolling
  ) where

import API.Requests    (getLongPollingServer, longPoll)
import API.Types       (LongPollServerSettings, Update, ts, updates, Failed(..))
import Bot.Types       (Bot)
import Bot.Handler     (HandlerState(..))
import BotPrelude      hiding (handle)

import Service.Logging (logError, logInfo)

startLongPolling :: (Update -> StateT HandlerState Bot ()) -> Bot ()
startLongPolling handle = do
  logInfo "Connecting to LongPoll server..."
  initialSettings <- getLongPollingServer
  logInfo $ "Got initial settings:\n" <> showT initialSettings
  maybe (pure ()) run initialSettings
  pure ()
    where
      run :: LongPollServerSettings -> Bot ()
      run settings = do
        _ <- runStateT (mkRequest settings) (HandlerState 0)
        pure ()

      mkRequest :: LongPollServerSettings -> StateT HandlerState Bot ()
      mkRequest settings = do
        res <- lift $ longPoll settings
        case res of
          Left (Failed 1 (Just newTs)) -> do
            logInfo "Got failed:1, making request with new TS..."
            mkRequest (settings & ts .~ newTs) 
          Left (Failed 2 _) -> do
            logInfo "Got failed:2, reconnecting to long poll server..."
            lift $ startLongPolling handle
          Left (Failed 3 _) -> do
            logInfo "Got failed:2, reconnecting to long poll server..."
            lift $ startLongPolling handle
          Left (Failed 4 _) -> do 
            logError "Got invalid version respone ('failed:4'). Please fix long poll verson!"
            pure ()

          Right Nothing -> do
            logError "Didn't get any result on long polling! Retrying in 5 seconds..."
            liftIO $ threadDelay 5000000
            mkRequest settings
          Right (Just u) -> do 
            mapM_ handle (u ^. updates)
            mkRequest (settings & ts .~ (u ^. ts))

          _ -> logError "Got unknown long poll result!" >> pure ()