module SlackEmotesTests
( spec ) where

import BotPrelude
import Test.Hspec

import Modules.SlackEmotes (zipLetters)

spec :: Spec
spec = describe "Module: SlackEmotes" $ do
 zippingLetters

zippingLetters :: Spec
zippingLetters =
    it "Zipps letters correctly" $ 
      zipLetters
      [ ["a", "b", "c"]
      , ["d", "e", "f"]
      ] 
      [ ["g", "h", "i"]
      , ["j", "k", "l"]
      ]
       `shouldBe` 
      [ ["a", "b", "c", " ", "g", "h", "i"]
      , ["d", "e", "f", " ", "j", "k", "l"]
      ]