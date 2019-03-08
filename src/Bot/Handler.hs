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
import qualified Modules.SlackEmotes as SlackEmotes 
import qualified VK.Requests as VK (sendMessage)

import Data.Attoparsec.Text 

import qualified Data.Text as T

getHandler :: Text -> (Integer -> Bot ())
getHandler input = processParsedResult (parse inputParser input)
  where 
    inputParser = asum
      [ Quiz.parser <?> "Quiz.parser"
      , ModesHelper.parser <?> "ModesHelper.parser"
      , SlackEmotes.parser <?> "SlackEmotes.parser"
      , parseRoman <?> "Roman parser"
      ]

    processParsedResult result = 
      case result of 
        Partial f -> processParsedResult (f T.empty)
        Done _ handler -> handler
        Fail remainingInput contexts err -> \userId -> 
          let
            msg = T.unlines $ 
              [ "Не удалось распарсить ввод."
              , ""
              , "Остаток ввода:"
              , remainingInput
              , ""
              , "Что сломалось: "
              ] ++ map T.pack contexts ++ 
              [ ""
              , "Ошибка:"
              , T.pack err
              ]
            in
          VK.sendMessage userId msg

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