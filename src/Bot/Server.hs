module Bot.Server
  ( runServer
  ) where

import           Bot.Config                (port)
import           Bot.Types                 (Bot, config)
import           BotPrelude
import           Network.Wai               (Application, Response, rawPathInfo,
                                            responseLBS)

import qualified Data.ByteString.Lazy      as LBS (fromStrict)
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Wai.Handler.Warp  (defaultSettings, runSettings,
                                            setPort)

import           Utils.Logging             (logInfo)

runServer :: Bot ()
runServer = do
  cfg <- (^. config) <$> ask

  let settings = setPort (cfg ^. port) defaultSettings

  logInfo $ "Starting server on port " <> showT (cfg ^. port)
  lift $ runSettings settings app

app :: Application
app request respond =
  let path = rawPathInfo request in
  respond $ case path of
    "/" -> index
    _   -> response404 $ path

index :: Response
index = responseLBS status200 [] "This is index"

response404 :: ByteString -> Response
response404 = (responseLBS status404 []) . LBS.fromStrict
