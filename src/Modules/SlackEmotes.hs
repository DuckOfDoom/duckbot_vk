module Modules.SlackEmotes 
( parser
)
where

import Bot.Types  (Bot)

import BotPrelude hiding (empty)
import Data.Attoparsec.Text (Parser, endOfInput)

import qualified VK.Requests as VK (sendMessage)

import Modules.SlackEmotes.Internal
import qualified Data.Text as T

parser :: Parser (Integer -> Bot ())
parser = 
  do
    (word, filled, empty) <- inputParser 
    endOfInput
    let out = (render . zipLetters empty) (map (mkLetter filled empty) (T.unpack word))
    pure (\userId -> VK.sendMessage userId out)