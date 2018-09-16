{-# LANGUAGE QuasiQuotes       #-}

module Modules.Quiz
  ( replyToMessage
  )
  where

import BotPrelude

import Prelude (lookup, (!!))

import Bot.Types          (Bot, getStateForUser, liftBot, quizState,
                           updateStateForUser)
import Modules.Quiz.Types (currentQuestion, score)
import System.Random      (randomRIO)
import Modules.Quiz.Types (defaultState)

import qualified Data.Text   as T (toLower)
import qualified VK.Requests as VK (sendMessage)
import qualified NeatInterpolation as F (text) 

import qualified Service.Logging as Log (info) 

mkQuestionMessage :: (Show b, Show a) => (a, b) -> Text -> Text
mkQuestionMessage sc q = 
  [F.text|Твой счет: ${curr}/${total}.
  Назови знаки в тональности: ${q}|]
  where
     curr = showT $ fst sc
     total = showT $ snd sc

data ScoreOperation = Reset | IncBoth | IncTotal

replyToMessage :: Integer -> Text -> Bot ()
replyToMessage userId text = do
  when (text == "/reset") resetState

  userState <- getStateForUser userId
  case (userState ^. quizState ^. currentQuestion) of
    Nothing -> do
      newQuestion <- getQuestion
      currScore <- updateState newQuestion identity
      VK.sendMessage userId $ mkQuestionMessage currScore newQuestion
    Just question -> 
      if isCorrectAnswer question text
        then do
          newQuestion <- getQuestion
          newScore <- updateState newQuestion (incScore IncBoth)
          VK.sendMessage userId $ "Правильно! " <> mkQuestionMessage newScore newQuestion
        else do
          newScore <- updateState question (incScore IncTotal)
          VK.sendMessage userId $ "Неправильно! " <> mkQuestionMessage newScore question
  where
    incScore :: ScoreOperation -> (Int, Int) -> (Int, Int)
    incScore IncBoth (curr, total) = (curr + 1, total + 1)
    incScore IncTotal (curr, total) = (curr, total + 1)
    incScore Reset _ = (0, 0)

    isCorrectAnswer :: Text -> Text -> Bool
    isCorrectAnswer question answer = fmap T.toLower (lookup question answers) == Just (T.toLower answer)

    getQuestion :: Bot Text
    getQuestion = do
      rand <- liftBot $ randomRIO (0, length answers - 1)
      pure $ fst (answers !! rand)

    updateState :: Text -> ((Int, Int) -> (Int, Int)) -> Bot (Int, Int)
    updateState newQuestion updateScore = do
      newSt <- updateStateForUser userId $ 
        (\st -> st & quizState %~
          (\qState -> qState
            & currentQuestion .~ Just newQuestion
            & score %~ updateScore))
      Log.info ("updated state: " <> showT newSt)
      pure (newSt ^. quizState ^. score)

    resetState = do
      _ <- updateStateForUser userId $ 
        (\st -> st & quizState .~ defaultState)
      pure ()

answers :: [(Text, Text)]
answers =
  [ ("C", "-" )
  , ("Am", "-")

  , ("G", "F")
  , ("Em", "F")

  , ("D", "F C")
  , ("Bm", "F C")

  , ("A", "F C G")
  , ("F#m", "F C G")

  , ("E", "F C G D")
  , ("C#m", "F C G D")

  , ("F", "B")
  , ("Dm", "B")

  , ("Bb", "B E")
  , ("Gm", "B E")

  , ("Eb", "B E A")
  , ("Cm", "B E A")

  , ("Ab", "B E A D")
  , ("Fm", "B E A D")
  ]

