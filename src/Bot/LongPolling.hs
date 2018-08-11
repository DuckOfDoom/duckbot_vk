module Bot.LongPolling 
  (startLongPolling
  ) where
  
import BotPrelude 
import Service.Wreq
import Bot.Types (Bot) 

startLongPolling :: Bot ()
startLongPolling = do
  _ <- lift $ forkIO $ poll'
  return ()
  
poll' :: IO () 
poll' = do 
  print "hey"
  threadDelay 1000
  poll'
