module JSONTests
  (run) where

import API.Types  (Response(..))
import BotPrelude
import Data.Aeson (decode)
import Test.Hspec

run :: IO ()
run = hspec $
  describe "'Response' parsing" $
    it "parses Error" $
      (decode errorJSON :: Maybe Response) `shouldBe` Just (Error 100 "herp")
  where
    errorJSON = "{\"error\":{\"error_code\":100, \"error_msg\":\"herp\"}}"
