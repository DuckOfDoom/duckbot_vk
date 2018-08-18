module JSONTests
  (run) where

import API.Types  (Error(..), LongPollServerSettings(..))
import BotPrelude

import API.Requests         (parse) -- TODO: Test this!
import Data.Aeson           (decode, encode)
import Data.ByteString.Lazy as LBS (toStrict)
import Data.HashMap.Strict  as HM (fromList)

import Test.Hspec

run :: IO ()
run = do 
  putStrLn $ showT $ LBS.toStrict $ encode $ ([fromList [("herp", "derp")], fromList [("hey", "wat")]] :: [HashMap Text Text])
  hspec $
    describe "parse json" $ do
      it "parses Error" $ 
        (decode errorJSON :: Maybe Error) `shouldBe` Just (Error 100 "herp" [fromList [("herp", "derp")], fromList [("hey", "wat")]])
      it "parses nested responses" $ 
        (decode lpsRequestJSON :: Maybe LongPollServerSettings) `shouldBe` Just (LongPollServerSettings "keyValue" "serverValue" "tsValue")
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}]}}"
    lpsRequestJSON = "{\"response\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}]}}"

