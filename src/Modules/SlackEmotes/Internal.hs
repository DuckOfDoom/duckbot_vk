module Modules.SlackEmotes.Internal
  ( render
  , inputParser
  , replaceChars
  , mkLetter
  , zipLetters
  ) where

import BotPrelude hiding (Word, option, empty)

import Data.Attoparsec.Text (Parser, manyTill, option, many1, satisfy, inClass, string, space, (<?>))
import Data.Attoparsec.Combinator (lookAhead)

import qualified Data.Text as T
import qualified Data.Char as C

import Modules.SlackEmotes.Letters (getLetter, availableLetters)

type Letter = [[Text]]
type Word = [[Text]]

inputParser :: Parser (Text, Text, Text)
inputParser = do
  let emoteName = mconcat <$> sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"] <?> "Input. emoteName"
  let defaultEmptyEmote = ":white_small_square:"
  -- Parse any text consisting of available letter and spaces until we get to " :" (start of the emote)
  text <- manyTill (satisfy (inClass (availableLetters ++ map C.toUpper availableLetters)) <|> space) (lookAhead $ string " :")
  -- Skip space and parse an emote name
  filled <- space *> emoteName
  -- Second emote is optional, so we insert white square if there is no second emote.
  empty <- option defaultEmptyEmote (space *> emoteName)
  pure ((T.toLower . T.pack) text, filled, empty)

-- Render lists as text
render :: Word -> Text
render = T.unlines . map mconcat

-- Make a letter by substituting emotes in lists
mkLetter :: Text -> Text -> Char -> Letter
mkLetter filled empty c = (replaceChars (getLetter c) filled empty)

-- Zip two letters together with a separator
zipLetters :: Text -> [Letter] -> Letter
zipLetters separator = fold0 zip2 
  where
    zip2 :: Letter -> Letter -> Letter 
    zip2 (x:xs) (y:ys) = (x ++ [separator] ++ y) : zip2 xs ys
    zip2 _ _ = []

-- Replace # and spaces in l with 
replaceChars :: Letter -> Text -> Text -> Letter
replaceChars letter filled empty = (map . map) replaceChars' letter
  where
    replaceChars' i = 
      case i of 
        "#" -> filled
        " " -> empty
        _ -> i