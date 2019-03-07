module JSONTests 
( spec )
where

import VK.Types  (Error(..), LongPollServerSettings(..))
import BotPrelude

import VK.Requests         (eitherParse)
import Data.HashMap.Strict  as HM (fromList)

import Test.Hspec

spec :: Spec
spec = --do
  -- putStrLn $ showT $ LBS.toStrict $ encode $ ([fromList [("herp", "derp")], fromList [("hey", "wat")]] :: [HashMap Text Text])
    describe "JSON parsing" $ do
      it "Parses Error" $
        (eitherParse errorJSON :: Either Error LongPollServerSettings) `shouldBe` Left (Error 100 "herp" [fromList [("herp", "derp")], fromList [("hey", "wat")]])
      it "Parses LongPollServerSettings" $
        (eitherParse lpsRequestJSON :: Either Error LongPollServerSettings) `shouldBe` Right (LongPollServerSettings "serverValue" "keyValue" 100500)
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}]}}"
    lpsRequestJSON = "{\"response\":{\"key\":\"keyValue\",\"server\":\"serverValue\",\"ts\":100500}}"

