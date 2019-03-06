module Modules.SlackEmotes 
( parser
, zipLetters
)
where

import BotPrelude 

import Data.Attoparsec.Text (Parser, char, parseTest, many1, satisfy, inClass, endOfInput, string, space, (<?>))
import Data.Text as T

test :: IO ()
test = parseTest parser "asd :monkas: :whats_up:" 

parser :: Parser Text
parser = do
  let someText = many1 (satisfy (inClass "A-Za-z")) <?> "Only letters!"
  let emoteName = mconcat <$> sequence [string ":", T.pack <$> many1 (satisfy (inClass "a-z_")), string ":"] <?> "Letters with '_' inside colons!"

  text <- someText 
  _ <- space
  emote1 <- emoteName
  _ <- space
  emote2 <- emoteName
  pure $ showT (text, emote1, emote2)

zipLetters :: [[Text]] -> [[Text]] -> [[Text]] 
zipLetters (x:xs) (y:ys) = (x ++ [" "] ++ y) : zipLetters xs ys
zipLetters [] [] = []

-- mkLetter :: Char -> [Text]
-- mkLetter = 