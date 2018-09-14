module Modules.Quiz
  ( replyToMessage
  )
  where

import BotPrelude

import Prelude (lookup, (!!))

import Bot.Types             (Bot, liftBot, quizState)
import Modules.Quiz.Types (currentQuestion)
import System.Random         (randomRIO)

import qualified Data.Text   as T (toLower, toUpper)
import qualified VK.Requests as VK (sendMessage)

replyToMessage :: Integer -> Text -> Bot ()
replyToMessage userId text = do
  qState <- fmap (^. quizState) get
  case (qState ^. currentQuestion) of
    Just q  -> processAnswer userId q text
    Nothing -> sendQuestion userId
  pure ()

type Question = Text
type Answer = Text

processAnswer :: Integer -> Question -> Answer -> Bot ()
processAnswer userId question answer
  | fmap T.toLower (lookup question answers) == Just (T.toLower answer) = do
    _ <- VK.sendMessage userId (mconcat ["Правильно! ", question, " -> ", answer])
    sendQuestion userId 

  | otherwise = do
    _ <- VK.sendMessage userId "Неправильно! Попробуй еще раз!"
    pure ()

sendQuestion :: Integer -> Bot ()
sendQuestion userId = do
  rand <- liftBot $ randomRIO (0, length answers - 1)
  let
    question = fst (answers !! rand)
    message = "Назови знаки в тональности: " <> fst (answers !! rand)

  updateState question
  _ <- VK.sendMessage userId message
  pure ()
    where
      updateState :: Text -> Bot ()
      updateState q = do
        st <- get
        let qState = st ^. quizState & currentQuestion .~ Just q
        put (st & quizState .~ qState)

answers :: [(Text, Text)]
answers = 
  [ ("C", "-" )
  , ("Am", "-")

  , ("G", "F")
  , ("Em", "F")

  , ("D", "F C")
  , ("Bm", "F C")

  , ("A", "F C G")
  , ("Fm", "F C G")

  , ("F", "B")
  , ("Dm", "B")

  , ("Bb", "B E")
  , ("Gm", "B E")

  , ("Eb", "B E A")
  , ("Eb", "B E A")
  ]

