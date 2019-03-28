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
import Control.Monad.Trans.Maybe (MaybeT, runMaybeT)

import qualified Data.Text    as T (unpack)
import qualified Network.Wreq as Wreq (Response, getWith, postWith, partText)
import qualified Service.Logging as Log (error)
import qualified NeatInterpolation as F 

class (MonadIO m, MonadReader Env m) => MonadWreq m
instance MonadWreq Bot where

-- Sends a GET request. 
getWith :: (MonadWreq m)
  => Text -- URL
  -> Options -- Wreq.Options
  -> m (Maybe LBS.ByteString) -- Response
getWith url opts = 
  runMaybeT $ handleException 
    ("GET " <> url)
    (Wreq.getWith 
      opts (T.unpack url)
    )

-- Sends a POST request. 
-- From Wreq docs:
-- Part and [Part] give a request body with a Content-Type of multipart/form-data. 
postWith :: (MonadWreq m) 
  => Text -- URL
  -> Options -- Wreq.Options
  -> [(Text, Text)] -- A list of data to send in form 
  -> m (Maybe LBS.ByteString) -- Response
postWith url opts args = 
 runMaybeT $ handleException
  ("POST " <> url)
  (Wreq.postWith 
    opts
    (T.unpack url)
    (map (\(x, y) -> Wreq.partText x y) args)
  )

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
