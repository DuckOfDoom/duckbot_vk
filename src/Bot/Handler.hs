module Bot.Handler
  ( handle
  )
  where

import VK.Types.Update (Update(..), fromUser, text)
import Bot.Types (Bot, liftBot, lastSentMessageId, getStateForUser)
import BotPrelude hiding (handle)

import qualified Service.Logging as Log (info)
import qualified Modules.Quiz as Quiz 
import qualified Modules.ModesHelper as ModesHelper 
import qualified VK.Requests as VK (sendMessage)

import Data.Attoparsec.Text

import qualified Data.Text as T

----------- PARSING
getHandler :: Text -> (Integer -> Bot ())
getHandler input = 
  case parseOnly parsers input of 
    Right handler -> handler
    Left err -> \userId -> VK.sendMessage userId $ "Не понимаю, о чем ты:\n" <> T.pack err 
  where 
    parsers = asum [Quiz.parser]

----------------------

handle :: Update -> Bot ()
handle m@Message{..} = do
  let mText = m ^?! text
  let userId = m ^?! fromUser
  let handler = getHandler mText

  lastSent <- (^. lastSentMessageId) <$> getStateForUser userId
  -- Ignore my own updates
  when (_messageId /= lastSent) $ do
    Log.info $ "Handler. Received Message: " <> showT m
    -- TODO: Handle with care
    -- getHandler mText userId mText
    handler userId
      -- where 
        -- getHandler :: Text -> Handler
        -- getHandler t = fromMaybe handleDefault $ msum $ map ($ t) 
        --   [ handleRoman
        --   , handleModes
        --   , handleQuiz
        --   ]

handle Undefined = pure ()

type Handler = Integer -> Text -> Bot ()

handleDefault :: Handler
handleDefault userId _ = VK.sendMessage userId "Не понимаю, о чем ты." 

-- handleModes :: Text -> Maybe Handler
-- handleModes t 
--   | "/mode" `T.isPrefixOf` t && (length . T.words) t >= 2 = Just 
--     (\userId msg -> 
--       case ModesHelper.getMode (getInput $ (drop 1 . T.words) msg) of 
--         Right result -> VK.sendMessage userId result 
--         Left error -> VK.sendMessage userId error
--     )
--   | otherwise = Nothing
--     where 
--       getInput (x:y:_) = (x, y)
--       getInput vals = ("Непонятный формат:", showT vals)

-- handleQuiz :: Text -> Maybe Handler
-- handleQuiz _ = pure Quiz.replyToMessage 

handleRoman :: Text -> Maybe Handler
handleRoman t 
  | hasPidor = Just (\userId _ -> do
    rand <- liftBot (randomIO :: IO Double)
    if rand < 0.1337
      then VK.sendMessage userId "вот ето у тебя пeчот)" 
      else VK.sendMessage userId "нет ты)))" 
  )
  | otherwise = Nothing
  where 
    hasPidor =
      let lowerT = T.toLower t
       in "ты хуй" `T.isInfixOf` lowerT 
       || "ты пидр" `T.isInfixOf` lowerT 
       || "нет ты" `T.isInfixOf` lowerT
