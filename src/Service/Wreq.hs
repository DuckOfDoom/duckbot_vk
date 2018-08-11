{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module Service.Wreq
  ( tryGetWith
  , tryPostWith
  ) where

import           BotPrelude
import           Control.Exception    (SomeException, try)
import           Data.ByteString.Lazy as LBS (ByteString)
import qualified Data.Text            as T (pack, unpack)
import           Network.Wreq         (Options, getWith, postWith)
import qualified Network.Wreq         as W (Response)
import           Network.Wreq.Types   (Postable)

import           Service.Logging      (HasLog, logError)

import           NeatInterpolation    (text)

class (MonadIO m, HasLog env, MonadReader env m) => MonadWreq env m 

tryGetWith :: (MonadWreq env m) => Options -> Text -> m (Either Text (W.Response LBS.ByteString))
tryGetWith options url = do
  response <- liftIO $ try (getWith options (T.unpack url)) :: (MonadWreq env m) => m (Either SomeException (W.Response LBS.ByteString))
  case response of
       Left ex -> do
         logError $ [text|Caught exception when trying to GET ${url}:\n${showT ex}|]
         pure $ Left (T.pack $ show ex)
       Right r -> pure $ Right r

tryPostWith :: (MonadWreq env m, Postable a) => Options -> Text -> a -> m (Either Text (W.Response LBS.ByteString))
tryPostWith options url postable = do
  response <- liftIO $ try (postWith options (T.unpack url) postable) :: (MonadWreq env m) => m (Either SomeException (W.Response LBS.ByteString))
  case response of
       Left ex -> do
         logError $ [text|Caught exception when trying to POST ${url}:\n${showT ex}|]
         pure $ Left (T.pack $ show ex)
       Right r -> pure $ Right r
  
