{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes           #-}

module Service.Wreq
  ( getWith
  , postWith
  ) where

import           Bot.Types                 (Env)
import           BotPrelude
import           Control.Exception         (SomeException, try)
import           Data.ByteString.Lazy      as LBS (ByteString)
import qualified Data.Text                 as T (unpack)
import           Network.Wreq              (Options, defaults)
import qualified Network.Wreq              as Wreq (Response, getWith, postWith)
import           Network.Wreq.Types        (Postable)

import           Service.Logging           (HasLog, logError)

import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           NeatInterpolation         (text)

class HasWreqOptions a where
  getOptions :: a -> Options

instance HasWreqOptions Env where
  getOptions _ = defaults -- TODO: Take options from env for real

class (MonadIO m, HasLog env, HasWreqOptions env, MonadReader env m) => MonadWreq env m

getWith :: (MonadWreq env m) => (Options -> Options) -> Text -> m (Maybe (Wreq.Response LBS.ByteString))
getWith patch url = do
  opts <- patch <$> getOptions <$> ask
  runMaybeT $ handleException ("GET " <> url) (Wreq.getWith opts (T.unpack url))

postWith :: (MonadWreq env m, Postable a) => (Options -> Options) -> Text -> a -> m (Maybe (Wreq.Response LBS.ByteString))
postWith patch url postable = do
 opts <- patch <$> getOptions <$> ask
 runMaybeT $ handleException ("POST " <> url) (Wreq.postWith opts (T.unpack url) postable)

handleException
 :: (MonadWreq env m)
 => Text
 -> IO (Wreq.Response LBS.ByteString)
 -> MaybeT m (Wreq.Response LBS.ByteString)
handleException source action = do
  result <- liftIO $ (try' $ action)
  case result of
    Left ex -> do
      let exMessage = showT ex
      logError [text|Caught exception when trying to ${source}:\n${exMessage}|]
      mzero
    Right r -> pure $ r
    where
      try' :: IO (a) -> IO (Either SomeException a)
      try' = try
