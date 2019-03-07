module SlackEmotesTests
( spec ) where

import BotPrelude
import Test.Hspec

import Modules.SlackEmotes (replaceChars, zipLetters, inputParser)
import Data.Attoparsec.Text (parseOnly)

spec :: Spec
spec = describe "Module: SlackEmotes" $ do
  inputParsing
  replacingCharacters
  zippingLetters

inputParsing :: Spec
inputParsing = 
  describe "inputParser" $ do
    it "Parses input correctly" $ 
      parseOnly inputParser "text :monkas: :hey_yo:" `shouldBe` Right ("text", ":monkas:", ":hey_yo:")
    it "Does not parse input with special symbols" $ 
      parseOnly inputParser "text text2" `shouldSatisfy` isLeft
    it "Does not parse incorrect input" $ do
      parseOnly inputParser "derp" `shouldSatisfy` isLeft
      parseOnly inputParser "text text2" `shouldSatisfy` isLeft
      parseOnly inputParser "text text1 text2" `shouldSatisfy` isLeft
      parseOnly inputParser "" `shouldSatisfy` isLeft

replacingCharacters :: Spec
replacingCharacters = 
  describe "replaceChars" $ do
    it "Replaces characters correctly" $ 
      replaceChars "hey" "yo" [[" ", "#", "#"], ["#", " ", " "]] `shouldBe` [["hey", "yo", "yo"], ["yo", "hey", "hey"]]
    it "Leaves incorrect chars as is" $ 
      let input = [["1", "2", "3"], ["1", "2", "3"]] in
      replaceChars "hey" "yo" input `shouldBe` input

zippingLetters :: Spec
zippingLetters =
  describe "zipLetters" $ 
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