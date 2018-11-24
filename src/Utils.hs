module Utils where

import BotPrelude

joinText :: Text -> [Text] -> Text
joinText sep = mconcat . intersperse sep