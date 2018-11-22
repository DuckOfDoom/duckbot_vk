module Bot.Handler
  ( handle
  )
  where

import VK.Types.Update (Update(..), fromUser, text)
import Bot.Types (Bot, liftBot, lastSentMessageId, getStateForUser)
import BotPrelude hiding (handle)

import qualified Service.Logging as Log (info)
import qualified Modules.Quiz as Quiz 
import qualified VK.Requests as VK (sendMessage)

import qualified Data.Text as T

handle :: Update -> Bot ()
handle m@Message{..} = do
  let mText = m ^?! text
  let userId = m ^?! fromUser

  lastSent <- (^. lastSentMessageId) <$> getStateForUser userId
  -- Ignore my own updates
  when (_messageId > lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
    -- TODO: Handle with care
    getHandler mText userId mText
      where 
        getHandler :: Text -> Handler
        getHandler t = fromMaybe handleDefault $ msum $ map ($ t) 
          [ handleRoman
          , handleQuiz
          ]

handle Undefined = pure ()

type Handler = Integer -> Text -> Bot ()

handleDefault :: Handler
handleDefault userId _ = VK.sendMessage userId "Не понимаю, о чем ты." 

handleQuiz :: Text -> Maybe Handler
handleQuiz _ = pure (\userId msg -> Quiz.replyToMessage userId msg)

handleRoman :: Text -> Maybe Handler
handleRoman t 
  | hasPidor = Just (\userId _ -> do
    rand <- liftBot (randomIO :: IO Double)
    if rand < 0.1337
      then VK.sendMessage userId "вот ето у тебя пeчот)" 
      else VK.sendMessage userId "нет ты)))" 
  )
  | otherwise = Nothing
  where 
    hasPidor =
      let lowerT = T.toLower t
       in "ты хуй" `T.isInfixOf` lowerT 
       || "ты пидр" `T.isInfixOf` lowerT 
       || "нет ты" `T.isInfixOf` lowerT
