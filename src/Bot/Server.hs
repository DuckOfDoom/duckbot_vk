module Bot.Server
  ( runServer
  ) where

import           Bot.Config                (port, confirmationString)
import           Bot.Types                 (Bot, config,Env)
import           BotPrelude
import           Network.Wai               (Application, Response, rawPathInfo,
                                            responseLBS)

import qualified Data.ByteString.Lazy      as LBS (fromStrict)
import           Network.HTTP.Types.Status (status200, status404)
import           Network.Wai.Handler.Warp  (defaultSettings, runSettings,
                                            setPort)

import           Service.Logging             (logInfo)

import           Data.Text.Encoding        (encodeUtf8)

runServer :: Bot ()
runServer = do
  env <- ask
  let 
    port' = env ^. (config . port)
    settings = setPort port' defaultSettings 
  
  logInfo $ "Starting server on port " <> showT port'
  lift $ runSettings settings (mkApp env)

mkApp :: Env -> Application
mkApp env request respond =
  let path = rawPathInfo request in
  respond $ case path of
    "/" -> index
    "/confirmation" -> confirm $ env ^. (config . confirmationString)
    _   -> response404 $ path

index :: Response
index = responseLBS status200 [] "This is index"

confirm :: Maybe Text -> Response
confirm = \case 
  Just t -> responseLBS status200 [] $ (LBS.fromStrict . encodeUtf8) t
  Nothing -> responseLBS status200 [] "Error. No confirmation value set!"

response404 :: ByteString -> Response
response404 = (responseLBS status404 []) . LBS.fromStrict
