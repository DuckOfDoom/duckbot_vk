module Modules.SlackEmotes.Internal
  ( render
  , inputParser
  , replaceChars
  , mkLetter
  , zipLetters
  ) where

import BotPrelude hiding (option)

import Data.Attoparsec.Text (Parser, option, many1, satisfy, inClass, string, space, (<?>))
import qualified Data.Text as T

import Modules.SlackEmotes.Letters (getLetter, availableLetters)

inputParser :: Parser (Text, Text, Text)
inputParser = do
  let someText = T.pack <$> many1 (satisfy (inClass availableLetters)) <?> "Input. someText"
  let emoteName = mconcat <$> sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"] <?> "Input. emoteName"
  
  text <- someText 
  emote1 <- space *> emoteName
  emote2 <- option " " (space *> emoteName)
  pure (text, emote1, emote2)

render :: [[Text]] -> Text
render = T.unlines . map mconcat

mkLetter :: Char -> Text -> Text -> [[Text]]
mkLetter c filled' empty' = (replaceChars filled' empty' (getLetter c))

zipLetters :: [[Text]] -> [[Text]] -> [[Text]] 
zipLetters (x:xs) (y:ys) = (x ++ [" "] ++ y) : zipLetters xs ys
zipLetters _ _ = []

replaceChars :: Text -> Text -> [[Text]] -> [[Text]]
replaceChars empty' filled' = (map . map) replaceChars'
  where
    replaceChars' i = 
      case i of 
        " " -> empty'
        "#" -> filled'
        _ -> i