module Modules.ModesHelper
 ( parser
 )
 where

import Modules.ModesHelper.Internal

import Bot.Types  (Bot)
import BotPrelude hiding (note, option, shift, take)
import qualified VK.Requests as VK (sendMessage)

import Data.Attoparsec.Text (Parser)

parser :: Parser (Integer -> Bot ())
parser = do
  (note, mode) <- parseInput
  pure (\userId -> VK.sendMessage userId (mkResponse note mode))