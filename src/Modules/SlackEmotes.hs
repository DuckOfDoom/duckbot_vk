module Modules.SlackEmotes 
( parser )
where

import BotPrelude 

import Data.Attoparsec.Text (Parser, parseTest)

test :: Text -> IO ()
test t = parseTest parser t 

parser :: Parser Text
parser = undefined