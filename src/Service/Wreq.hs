{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE FlexibleInstances #-}

module Service.Wreq
  ( getWith
  , postWith
  , module Network.Wreq 
  ) where

import Bot.Types            (Bot, Env)
import BotPrelude
import Control.Exception    (SomeException, try)
import Data.ByteString.Lazy as LBS (ByteString)
import Network.Wreq         (Options, defaults, param, responseBody)
import Network.Wreq.Types   (Postable)
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import qualified Data.Text    as T (unpack)
import qualified Network.Wreq as Wreq (Response, getWith, postWith)
import qualified Service.Logging as Log (error)
import qualified NeatInterpolation as F 

class (MonadIO m, MonadReader Env m) => MonadWreq m
instance MonadWreq Bot where

getWith :: (MonadWreq m) => Text -> Options -> m (Maybe LBS.ByteString)
getWith url opts = 
  runMaybeT $ handleException ("GET " <> url) (Wreq.getWith opts (T.unpack url))

postWith :: (MonadWreq m, Postable a) => Text -> Options -> a -> m (Maybe LBS.ByteString)
postWith url opts postable = 
 runMaybeT $ handleException ("POST " <> url) (Wreq.postWith opts (T.unpack url) postable)

handleException :: (MonadWreq m)
 => Text
 -> IO (Wreq.Response LBS.ByteString)
 -> MaybeT m LBS.ByteString
handleException source action = do
  result <- liftIO (try' action)
  case result of
    Left ex -> do
      let exMessage = showT ex
      Log.error [F.text|Caught exception when trying to ${source}:
      ${exMessage}|]
      mzero
    Right r -> pure (r ^. responseBody)
    where
      try' :: IO a -> IO (Either SomeException a)
      try' = try
