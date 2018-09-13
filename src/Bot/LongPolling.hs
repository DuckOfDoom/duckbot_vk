module Bot.LongPolling
  ( startLongPolling
  ) where

import Bot.Types   (Bot)
import BotPrelude  hiding (handle)
import VK.Requests (getLongPollingServer, longPoll)
import VK.Types    (Failed(..), LongPollServerSettings, Update, ts, updates)

import qualified Service.Logging as Log (error, info)

startLongPolling :: (Update -> Bot ()) -> Bot ()
startLongPolling handle = do
  Log.info "Connecting to LongPoll server..."
  initialSettings <- getLongPollingServer
  Log.info $ "Got initial settings:\n" <> showT initialSettings
  maybe (pure ()) run initialSettings
  pure ()
    where
      run :: LongPollServerSettings -> Bot ()
      run settings = do
        _ <- mkRequest settings
        pure ()

      mkRequest :: LongPollServerSettings -> Bot ()
      mkRequest settings = do
        res <- longPoll settings
        case res of
          Left (Failed 1 (Just newTs)) -> do
            Log.info "Got failed:1, making request with new TS..."
            mkRequest (settings & ts .~ newTs)
          Left (Failed 2 _) -> do
            Log.info "Got failed:2, reconnecting to long poll server..."
            startLongPolling handle
          Left (Failed 3 _) -> do
            Log.info "Got failed:2, reconnecting to long poll server..."
            startLongPolling handle
          Left (Failed 4 _) -> do
            Log.error "Got invalid version respone ('failed:4'). Please fix long poll verson!"
            pure ()

          Right Nothing -> do
            Log.error "Didn't get any result on long polling! Retrying in 5 seconds..."
            liftIO $ threadDelay 5000000
            mkRequest settings
          Right (Just u) -> do
            mapM_ handle (u ^. updates)
            mkRequest (settings & ts .~ (u ^. ts))

          _ -> Log.error "Got unknown long poll result!" >> pure ()
