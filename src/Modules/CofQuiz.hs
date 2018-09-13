module Modules.CofQuiz
  ( replyToMessage
  )
  where

import BotPrelude

import Prelude (lookup, (!!))

import System.Random (randomRIO)
import Bot.Types (Bot, quizState)
import Modules.CofQuiz.Types (currentQuestion, QuizState)

import qualified VK.Requests as VK (sendMessage)

replyToMessage :: Integer -> Text -> Bot ()
replyToMessage userId text = do
  qState <- fmap (^. quizState) ask
  case (qState ^. currentQuestion) of
    Just q -> processAnswer userId q text
    Nothing -> sendQuestion userId
  pure ()

type Question = Text
type Answer = Text

processAnswer :: Integer -> Question -> Answer -> Bot ()
processAnswer userId question answer
  | lookup question answers == Just answer = do
    _ <- VK.sendMessage userId "Correct!"
    sendQuestion userId

  | otherwise = do
    _ <- VK.sendMessage userId "Incorrect! Try again!"
    pure ()

sendQuestion :: Integer -> Bot ()
sendQuestion userId = do
  rand <- lift $ randomRIO (0, length answers - 1)
  let
    question = fst (answers !! rand)
    message = "Вот вопрос: " <> fst (answers !! rand)

  _ <- updateState question . (^. quizState) <$> ask
  _ <- VK.sendMessage userId message
  pure ()
    where
      updateState :: Text -> QuizState -> QuizState
      updateState q st = st & currentQuestion .~ (Just q)

answers :: [(Text, Text)]
answers =
  [ ("C", "-" )
  , ("Am", "-")

  , ("G", "F#")
  , ("Em", "F#")

  , ("D", "F# C#")
  , ("Bm", "F# C#")

  , ("A", "F# C# G#")
  , ("Fm", "F# C# G#")

  , ("F", "Bb")
  , ("Dm", "Bb")

  , ("Bb", "Bb Eb")
  , ("Gm", "Bb Eb")

  , ("Eb", "Bb Eb Db")
  , ("Eb", "Bb Eb Db")
  ]

