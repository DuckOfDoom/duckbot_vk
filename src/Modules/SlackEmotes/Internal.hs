module Modules.SlackEmotes.Internal
  ( render
  , inputParser
  , replaceChars
  , mkLetter
  , zipLetters
  ) where

import BotPrelude hiding (option, empty)

import Data.Attoparsec.Text (Parser, option, many1, satisfy, inClass, string, space, (<?>))
import qualified Data.Text as T
import qualified Data.Char as C

import Modules.SlackEmotes.Letters (getLetter, availableLetters)

inputParser :: Parser (Text, Text, Text)
inputParser = do
  let someText = T.pack <$> many1 (satisfy (inClass (availableLetters ++ map C.toUpper availableLetters))) <?> "Input. someText"
  let emoteName = mconcat <$> sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"] <?> "Input. emoteName"
  let defaultEmptyEmote = ":white_small_square:"
  
  text <- someText 
  filled <- space *> emoteName
  empty <- option defaultEmptyEmote (space *> emoteName)
  pure (T.toLower text, filled, empty)

render :: [[Text]] -> Text
render = T.unlines . map mconcat

mkLetter :: Text -> Text -> Char -> [[Text]]
mkLetter filled empty c = (replaceChars filled empty (getLetter c))

zipLetters :: Text -> [[[Text]]] -> [[Text]]
zipLetters separator = fold0 zip2 
  where
    zip2 :: [[Text]] -> [[Text]] -> [[Text]] 
    zip2 (x:xs) (y:ys) = (x ++ [separator] ++ y) : zip2 xs ys
    zip2 _ _ = []

replaceChars :: Text -> Text -> [[Text]] -> [[Text]]
replaceChars filled empty = (map . map) replaceChars'
  where
    replaceChars' i = 
      case i of 
        "#" -> filled
        " " -> empty
        _ -> i