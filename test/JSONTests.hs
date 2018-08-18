module JSONTests
  (run) where

import API.Types           (Error(..))
import BotPrelude
import Data.Aeson          (decode, encode)
import Data.HashMap.Strict as HM (--empty, singleton,  
  fromList)
import Data.ByteString.Lazy as LBS (toStrict)

import Test.Hspec

run :: IO ()
run = do
  putStrLn $ showT $ LBS.toStrict $ encode $ ([fromList [("herp", "derp")], fromList [("hey", "wat")]] :: [HashMap Text Text])
  hspec $
    describe "'Response' parsing" $
      it "parses Error" $
        (decode errorJSON :: Maybe Error) `shouldBe` Just (Error 100 "herp" [fromList [("herp", "derp")], fromList [("hey", "wat")]])
      -- it "parses misc" $
      --     (decode "[{\"a\": \"b\"}, {\"c\": \"d\"}]" :: Maybe (HashMap Text Text)) `shouldBe` (Just $ HM.fromList [("a", "b"), ("c", "d")] )
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}}]}"
