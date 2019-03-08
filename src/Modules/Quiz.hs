{-# LANGUAGE QuasiQuotes #-}

module Modules.Quiz
  ( parser
  )
  where

import BotPrelude
import Prelude    (lookup, (!!))

import Bot.Types          (Bot, getStateForUser, liftBot, quizState,
                           updateStateForUser)
import Modules.Quiz.Types (currentQuestion, defaultState, score)

import Data.List      (nub)
import VK.Types       (Keyboard)
import VK.Types.Utils (mkKeyboard)

import qualified Data.Text         as T (toLower)
import qualified NeatInterpolation as F (text)
import qualified Service.Logging   as Log (info)
import qualified VK.Requests       as VK (sendMessageWithKeyboard)

import Data.Attoparsec.Text (Parser, endOfInput, string, (<?>))

parser :: Parser (Integer -> Bot ())
parser = do
  parsedAnswer <- asum (string "/reset" : map ((<* endOfInput) . string) (map snd answers)) <?> "Input"
  pure $ replyToMessage parsedAnswer

mkQuestionMessage :: (Show b, Show a) => (a, b) -> Text -> Text
mkQuestionMessage sc q =
  [F.text|Твой счет: ${curr}/${total}.
  Назови знаки в тональности: ${q}|]
  where
     curr = showT $ fst sc
     total = showT $ snd sc

data ScoreOperation = Reset | IncBoth | IncTotal

replyToMessage :: Text -> Integer -> Bot ()
replyToMessage text userId = do
  when (text == "/reset") resetState

  userState <- getStateForUser userId
  case userState ^. quizState ^. currentQuestion of
    Nothing -> do
      newQuestion <- getQuestion
      currScore <- updateState newQuestion identity
      VK.sendMessageWithKeyboard userId (mkQuestionMessage currScore newQuestion) answersKeyboard
    Just question ->
      if isCorrectAnswer question text
        then do
          newQuestion <- getQuestion
          newScore <- updateState newQuestion (incScore IncBoth)
          VK.sendMessageWithKeyboard
            userId
            ("Правильно! " <> mkQuestionMessage newScore newQuestion)
            answersKeyboard
        else do
          newScore <- updateState question (incScore IncTotal)
          VK.sendMessageWithKeyboard
            userId
            ("Неправильно! " <> mkQuestionMessage newScore question)
            answersKeyboard
  where
    incScore :: ScoreOperation -> (Int, Int) -> (Int, Int)
    incScore IncBoth (curr, total)  = (curr + 1, total + 1)
    incScore IncTotal (curr, total) = (curr, total + 1)
    incScore Reset _                = (0, 0)

    isCorrectAnswer :: Text -> Text -> Bool
    isCorrectAnswer question answer = fmap T.toLower (lookup question answers) == Just (T.toLower answer)

    getQuestion :: Bot Text
    getQuestion = do
      rand <- liftBot $ randomRIO (0, length answers - 1)
      pure $ fst (answers !! rand)

    updateState :: Text -> ((Int, Int) -> (Int, Int)) -> Bot (Int, Int)
    updateState newQuestion updateScore = do
      newSt <- updateStateForUser userId
        (\st -> st & quizState %~
          (\qState -> qState
            & currentQuestion .~ Just newQuestion
            & score %~ updateScore))
      Log.info ("Updated state: " <> showT newSt)
      pure (newSt ^. quizState ^. score)

    resetState = do
      _ <- updateStateForUser userId
        (\st -> st & quizState .~ defaultState)
      pure ()

answers :: [(Text, Text)]
answers =
  [ ("C", "-" )
  , ("Am", "-")

  , ("G", "F#")
  , ("Em", "F#")

  , ("D", "F# C#")
  , ("Bm", "F# C#")

  , ("A", "F# C# G#")
  , ("F#m", "F# C# G#")

  , ("E", "F# C# G# D#")
  , ("C#m", "F# C# G# D#")

  , ("F", "Bb")
  , ("Dm", "Bb")

  , ("Bb", "Bb Eb")
  , ("Gm", "Bb Eb")

  , ("Eb", "Bb Eb Ab")
  , ("Cm", "Bb Eb Ab")

  , ("Ab", "Bb Eb Ab Db")
  , ("Fm", "Bb Eb Ab Db")
  ]

answersKeyboard :: Keyboard
answersKeyboard =
  let answers' = (nub . map snd) answers in
  mkKeyboard
    [ (take 4 . drop 1) answers'
    , (take 4 . drop 5) answers'
    , "/reset" : take 1 answers'
    ]
