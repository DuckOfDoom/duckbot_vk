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
    replied <- canHandleRoman mText userId
    unless replied $ Quiz.replyToMessage (m ^?! fromUser) (m ^?! text)

handle Undefined = pure ()

canHandleRoman :: Text -> Integer -> Bot Bool
canHandleRoman t userId 
  | hasPidor = do
    rand <- liftBot (randomIO :: IO Double)
    if rand < 0.1337
      then VK.sendMessage userId "вот ето у тебя пeчот)" 
      else VK.sendMessage userId "нет ты)))" 
    pure True
  | otherwise = 
      pure False
  where 
    hasPidor =
      let lowerT = T.toLower t
       in "ты хуй" `T.isInfixOf` lowerT 
       || "ты пидр" `T.isInfixOf` lowerT 
       || "нет ты" `T.isInfixOf` lowerT