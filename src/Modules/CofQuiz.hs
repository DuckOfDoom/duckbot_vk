module Modules.CofQuiz
  ( replyToMessage
  )
  where

import BotPrelude
import Bot.Types (Bot, quizState)
import Modules.CofQuiz.Types (currentQuestion)

import qualified VK.Requests as VK (sendMessage)

replyToMessage :: Integer -> Text -> Bot ()
replyToMessage userId text = do
  state <- fmap (^. quizState) ask
  case (state ^. currentQuestion) of 
    Just q -> processAnswer userId q text
    Nothing -> sendQuestion 
  pure ()

  -- main = do
  --   r <- randomRIO (0, length(answers) - 1)
  --   print $ answers !! r
  --   print $ lookup "a" answers

type Question = Text
type Answer = Text

processAnswer :: Integer -> Question -> Answer -> Bot ()
processAnswer = undefined

sendQuestion :: Bot () 
sendQuestion = undefined
    
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
