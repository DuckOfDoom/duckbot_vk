module API.Requests
  ( getLongPollingServer
  ) where

import           BotPrelude

import           Bot.Types           (Bot)
import           Service.UrlComposer (getLongPoll)
import           Service.Wreq        (getWith)

getLongPollingServer :: Bot (Maybe (Text, Text, Text))
getLongPollingServer = undefined 
--do
--  result <- getWith getLongPoll
  
