module Modules.ModesHelper
 ( parser
 )
 where

import Modules.ModesHelper.Internal

import Bot.Types  (Bot)
import BotPrelude hiding (note, option, shift, take)
import qualified VK.Requests as VK (sendMessage)

import Data.Attoparsec.Text (Parser, space, (<?>))

parser :: Parser (Integer -> Bot ())
parser = do
  note <- parseNote <?> "Note parser"
  mode <- space >> parseMode <?> "Mode parser"
  pure (\userId -> VK.sendMessage userId (mkResponse note mode))