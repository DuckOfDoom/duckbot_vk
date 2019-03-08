module Modules.SlackEmotes 
( parser
)
where

import Bot.Types  (Bot)

import BotPrelude 
import Data.Attoparsec.Text (Parser, endOfInput)

import qualified VK.Requests as VK (sendMessage)

import Modules.SlackEmotes.Internal

parser :: Parser (Integer -> Bot ())
parser = 
  do
    (_, empty', filled') <- inputParser 
    endOfInput
    let out = render $ mkLetter 'a' empty' filled'
    pure (\userId -> VK.sendMessage userId (showT out))
