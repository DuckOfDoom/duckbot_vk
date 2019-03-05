module Modules.SlackEmotes 
( parser )
where

import BotPrelude 

import Data.Attoparsec.Text (Parser, char, parseTest, many1, satisfy, inClass, endOfInput, string, space)
import Data.Text as T

test :: IO ()
test = parseTest parser "\"asd\" :monkas: :whats_up:"

parser :: Parser Text
parser = do
  let someText = many1 (satisfy (inClass "A-Za-z"))
  let emoteName = sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"]

  text <- char '\"' *> someText <* char '\"'
  _ <- space
  emote1 <- emoteName
  _ <- space
  emote2 <- emoteName
  -- endOfInput
  pure $ showT (text, emote1, emote2)