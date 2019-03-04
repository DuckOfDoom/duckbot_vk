module Bot.Handler
  ( handle
  )
  where

import VK.Types.Update (Update(..), fromUser, text)
import Bot.Types (Bot, liftBot, lastSentMessageId, getStateForUser)
import BotPrelude hiding (handle)

import qualified Service.Logging as Log (info)
import qualified Modules.Quiz as Quiz 
import qualified Modules.ModesHelper as ModesHelper 
import qualified VK.Requests as VK (sendMessage)

import Data.Attoparsec.Text 

import qualified Data.Text as T

getHandler :: Text -> (Integer -> Bot ())
getHandler input = 
  case parseOnly inputParser input of 
    Right handler -> handler
    Left err -> \userId -> VK.sendMessage userId $ "Невалидный ввод:\n" <> T.pack err 
  where 
   inputParser = asum
      [ Quiz.parser
      , ModesHelper.parser
      , parseRoman
      ]

handle :: Update -> Bot ()
handle m@Message{..} = do
  let mText = m ^?! text
  let userId = m ^?! fromUser
  let handler = getHandler mText

  lastSent <- (^. lastSentMessageId) <$> getStateForUser userId
  -- Ignore my own updates
  when (_messageId /= lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
    handler userId

handle Undefined = pure ()

parseRoman :: Parser (Integer -> Bot ())
parseRoman = do
  _ <- string "ты хуй" <|> string "ты пидр" <|> string "нет ты"
  pure (\userId -> do
    rand <- liftBot (randomIO :: IO Double)
    if rand < 0.1337
      then VK.sendMessage userId "вот ето у тебя пeчот)" 
      else VK.sendMessage userId "нет ты)))"
    )