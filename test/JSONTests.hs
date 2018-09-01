module JSONTests
  (run) where

import VK.Types  (Error(..), LongPollServerSettings(..))
import BotPrelude

import VK.Requests         (eitherParse)
import Data.HashMap.Strict  as HM (fromList)

import Test.Hspec

run :: IO ()
run = --do
  -- putStrLn $ showT $ LBS.toStrict $ encode $ ([fromList [("herp", "derp")], fromList [("hey", "wat")]] :: [HashMap Text Text])
  hspec $
    describe "JSON parsing" $ do
      it "parses Error" $
        (eitherParse errorJSON :: Either Error LongPollServerSettings) `shouldBe` Left (Error 100 "herp" [fromList [("herp", "derp")], fromList [("hey", "wat")]])
      it "parses LongPollServerSettings" $
        (eitherParse lpsRequestJSON :: Either Error LongPollServerSettings) `shouldBe` Right (LongPollServerSettings "keyValue" "serverValue" 100500)
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}]}}"
    lpsRequestJSON = "{\"response\":{\"key\":\"keyValue\",\"server\":\"serverValue\",\"ts\":100500}}"

