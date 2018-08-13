{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE QuasiQuotes       #-}

module Service.Wreq
  ( getWith
  , postWith
  ) where

import           Bot.Config                (accessToken, apiVersion)
import           Bot.Types                 (Bot, Env, config)
import           BotPrelude
import           Control.Exception         (SomeException, try)
import           Data.ByteString.Lazy      as LBS (ByteString)
import qualified Data.Text                 as T (unpack)
import           Network.Wreq              (Options, defaults, param,
                                            responseBody)
import qualified Network.Wreq              as Wreq (Response, getWith, postWith)
import           Network.Wreq.Types        (Postable)

import           Service.Logging           (logError)

import           Control.Monad.Trans.Maybe (MaybeT, runMaybeT)
import           NeatInterpolation         (text)

class HasWreqOptions a where
  getOptions :: a -> Options

instance HasWreqOptions Env where
  getOptions e = defaults
    & param "access_token" .~ [e ^. (config . accessToken)]
    & param "v"            .~ [showT $ e ^. (config . apiVersion)]

class (MonadIO m, MonadReader Env m) => MonadWreq m
instance MonadWreq Bot

getWith :: (MonadWreq m) => Text -> (Options -> Options) -> m (Maybe LBS.ByteString)
getWith url patch = do
  opts <- patch <$> getOptions <$> ask
  runMaybeT $ handleException ("GET " <> url) (Wreq.getWith opts (T.unpack url))

postWith :: (MonadWreq m, Postable a) => Text -> (Options -> Options) -> a -> m (Maybe LBS.ByteString)
postWith url patch postable = do
 opts <- patch <$> getOptions <$> ask
 runMaybeT $ handleException ("POST " <> url) (Wreq.postWith opts (T.unpack url) postable)

handleException
 :: (MonadWreq m)
 => Text
 -> IO (Wreq.Response LBS.ByteString)
 -> MaybeT m LBS.ByteString
handleException source action = do
  result <- liftIO $ (try' $ action)
  case result of
    Left ex -> do
      let exMessage = showT ex
      logError [text|Caught exception when trying to ${source}:\n${exMessage}|]
      mzero
    Right r -> pure $ (r ^. responseBody)
    where
      try' :: IO (a) -> IO (Either SomeException a)
      try' = try
