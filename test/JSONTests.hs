module JSONTests
  (run) where

import API.Types  (Error(..), LongPollServerSettings(..))
import BotPrelude

import API.Requests         (parseResponse)
import Data.Aeson           (encode)
import Data.ByteString.Lazy as LBS (toStrict)
import Data.HashMap.Strict  as HM (fromList)

import Test.Hspec

run :: IO ()
run = --do
  -- putStrLn $ showT $ LBS.toStrict $ encode $ ([fromList [("herp", "derp")], fromList [("hey", "wat")]] :: [HashMap Text Text])
  hspec $
    describe "JSON parsing" $ do
      it "parses Error" $
        (parseResponse errorJSON :: Either Error LongPollServerSettings) `shouldBe` Left (Error 100 "herp" [fromList [("herp", "derp")], fromList [("hey", "wat")]])
      it "parses LongPollServerSettings" $
        (parseResponse lpsRequestJSON :: Either Error LongPollServerSettings) `shouldBe` Right (LongPollServerSettings "keyValue" "serverValue" "tsValue")
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}]}}"
    lpsRequestJSON = "{\"response\":{\"key\":\"keyValue\",\"server\":\"serverValue\",\"ts\":\"tsValue\"}}"

