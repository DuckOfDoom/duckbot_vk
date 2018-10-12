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
  let userId = m ^?! fromUser
  lastSent <- (^. lastSentMessageId) <$> getStateForUser userId
  -- Ignore my own updates
  when (_messageId > lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
    let mText = m ^?! text
    if "ты хуй" `T.isInfixOf` T.toLower mText || "нет ты" `T.isInfixOf` T.toLower mText
      then do
        rand <- liftBot (randomIO :: IO Double)
        if rand < 0.1337
          then VK.sendMessage userId "вот ето у тебя пeчот)"
          else VK.sendMessage userId "нет ты)))"
        -- TODO: Rewrite Updates without sum types so we don't need to use fromJust?
      else 
        Quiz.replyToMessage (m ^?! fromUser) (m ^?! text)
    -- VK.sendMessage _fromUser ("Don't you '" <> _text <> "' on me!")
    pure () 

handle Undefined = pure ()
  