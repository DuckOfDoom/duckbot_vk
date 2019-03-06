module Modules.SlackEmotes 
( parser
, inputParser
, zipLetters
)
where

import Bot.Types  (Bot)

import BotPrelude 

import Data.Attoparsec.Text (Parser, parseTest, many1, satisfy, inClass, string, space, (<?>))
import Data.Text as T

parser :: Parser (Integer -> Bot ())
parser = undefined

inputParser :: Parser (Text, Text, Text)
inputParser = do
  let someText = many1 (satisfy (inClass "A-Za-z")) <?> "Only letters!"
  let emoteName = mconcat <$> sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"] <?> "Letters with '_' inside colons!"
  
  text <- someText 
  _ <- space
  emote1 <- emoteName
  _ <- space
  emote2 <- emoteName
  pure (text, emote1, emote2)

zipLetters :: [[Text]] -> [[Text]] -> [[Text]] 
zipLetters (x:xs) (y:ys) = (x ++ [" "] ++ y) : zipLetters xs ys
zipLetters _ _ = []

-- mkLetter :: Char -> [Text]
-- mkLetter = 