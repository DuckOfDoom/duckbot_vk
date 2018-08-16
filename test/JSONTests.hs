module JSONTests
  (run) where

import API.Types           (Response(..))
import BotPrelude
import Data.Aeson          (decode, encode)
import Data.HashMap.Strict as HM (empty, fromList, singleton)

import Test.Hspec

run :: IO ()
run = do
  putStrLn $ showT $ lbsToStrict $ encode $ ([HM.singleton "a" "b", HM.singleton "c" "d"] :: [HashMap Text Text])
  hspec $
    describe "'Response' parsing" $
      -- it "parses Error" $
      --   (decode errorJSON :: Maybe Response) `shouldBe` Just (Error 100 "herp" [("a", "b")] )
      it "parses misc" $
          (decode "[{\"a\": \"b\"}, {\"c\": \"d\"}]" :: Maybe (HashMap Text Text)) `shouldBe` (Just $ HM.fromList [("a", "b"), ("c", "d")] )
  where
    -- errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\", \"request_params\": [{\"herp\":\"derp\"}, {\"hey\": \"wat\"}}]}"

