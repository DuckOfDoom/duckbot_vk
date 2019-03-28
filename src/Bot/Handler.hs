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
import qualified Utils

getHandler :: Text -> (Integer -> Bot ())
getHandler input = processParsedResult (parse inputParser input)
  where 
    inputParser = asum
      [ Quiz.parser <?> "Quiz.parser"
      , ModesHelper.parser <?> "ModesHelper.parser"
      , SlackEmotes.parser <?> "SlackEmotes.parser"
      , parseRoman <?> "Roman parser"
      , help
      ]

    processParsedResult result = 
      case result of 
        Partial f -> processParsedResult (f T.empty)
        Done _ handler -> handler
        Fail remainingInput _ err -> \userId -> 
          let
            msg = T.unlines $ 
              [ "Не удалось распарсить ввод."
              , ""
              , "Остаток ввода:"
              , remainingInput
              , ""
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

help :: Parser (Integer -> Bot())
help = string "/help" <|> string "/?" >>
  pure (\userId -> VK.sendMessage userId $ 
    Utils.joinText "\n" 
      [ "Привет!"
      , ""
      , "Я умею следующие штуки:"
      , ""
      , "1. Могу экзаменовать тебя по знакам в тональностях. Пока только до четырех знаков."
      , "   Чтобы начать или сбросить результаты, набери '/reset'. Ответы можно давать с клавиатуры."
      , ""
      , "2. Могу помочь построить какой-нибудь диатонический лад от любой ноты."
      , "   Просто набери имя ноты, а через пробел первые три или больше буков названия нужного тебе лада (латинскими буквами)."
      , "   Примеры: \"C maj\", \"Ab dorian\", \"F# mixo\""
      , ""
      , "3. Могу слепить слово из эмоутов для Slack (типа ASCII-арт, но не совсем :D). Только латинские буквы, A-Z."
      , "   Синтаксис: СЛОВО :эмоут_для_заполнненной_клетки: :эмоут_для_пустой_клетки: "
      , "   Второй эмоут можно опустить, по умолчанию он заменится на :e:"
      , "   ВНИМАНИЕ: Сообщения от бота могут быть слишком длинными при пользовании этой функцией, поэтому бот может тупо не ответить, если слово слишком длинное."
      , "   Я работаю над этим."
      ]
    )