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

tryGetWith :: Options -> Text -> IO (Either Text (W.Response LBS.ByteString))
tryGetWith options url = do
  response <- try (getWith options (T.unpack url)) :: IO (Either SomeException (W.Response LBS.ByteString))
  case response of
       Left ex -> return $ Left (T.pack $ show ex)
       Right r -> return $ Right r

tryPostWith :: Postable a => Options -> Text -> a -> IO (Either Text (W.Response LBS.ByteString))
tryPostWith options url postable = do
  response <- try (postWith options (T.unpack url) postable) :: IO (Either SomeException (W.Response LBS.ByteString))
  case response of
       Left ex -> return $ Left (T.pack $ show ex)
       Right r -> return $ Right r

