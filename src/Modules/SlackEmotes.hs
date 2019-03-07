module Modules.SlackEmotes 
( parser
 -- For tests
, inputParser
, replaceChars
, mkLetter
, zipLetters
)
where

import Bot.Types  (Bot)

import BotPrelude 
import Data.HashMap.Strict (fromList, lookup) 

import Data.Attoparsec.Text (Parser, many1, satisfy, inClass, string, space, (<?>))
import qualified Data.Text as T
-- import Data.Char as C

parser :: Parser (Integer -> Bot ())
parser = undefined

inputParser :: Parser (Text, Text, Text)
inputParser = do
  let someText = T.pack <$> many1 (satisfy (inClass "A-Za-z")) <?> "Only letters!"
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

replaceChars :: Text -> Text -> [[Text]] -> [[Text]]
replaceChars empt filled = (map . map) replaceChars'
  where
    replaceChars' i = 
      case i of 
        " " -> empt
        "#" -> filled
        _ -> i

mkLetter :: Char -> Text -> Text -> Maybe [[Text]]
mkLetter c empty' filled' = 
  case lookup c letters of 
    Nothing -> Nothing
    Just t -> Just (replaceChars empty' filled' t)
  
letters :: HashMap Char [[Text]]
letters = fromList
  [('a'
  ,[[" "," ","#"," "," "]
   ,[" ","#"," ","#"," "]
   ,["#"," "," "," ","#"]
   ,["#","#","#","#","#"]
   ,["#"," "," "," ","#"]]
   )
  ]