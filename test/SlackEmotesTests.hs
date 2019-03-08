module SlackEmotesTests
( spec ) where

import BotPrelude
import Test.Hspec

import Modules.SlackEmotes.Internal (replaceChars, zipLetters, inputParser)
import Data.Attoparsec.Text (parseOnly)
import qualified Data.Text as T

spec :: Spec
spec = describe "Module: SlackEmotes" $ do
  inputParsing
  replacingCharacters
  zippingLetters

inputParsing :: Spec
inputParsing = 
  describe "inputParser" $ do
    it "Parses input correctly" $ do
      let input = "ab"
      parseOnly inputParser (input <> " :monkas: :hey_yo:") `shouldBe` Right (input, ":monkas:", ":hey_yo:")
      -- should treat any case as lower (we have only one alphabet anyways)
      parseOnly inputParser (T.toUpper input <> " :monkas: :hey_yo:") `shouldBe` Right (input, ":monkas:", ":hey_yo:")
      parseOnly inputParser "a :monkas:" `shouldBe` Right ("a", ":monkas:", ":white_small_square:")
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
      replaceChars "hey" "yo" [[" ", "#", "#"], ["#", " ", " "]] `shouldBe` [["yo", "hey", "hey"], ["hey", "yo", "yo"]]
    it "Leaves incorrect chars as is" $ 
      let input = [["1", "2", "3"], ["1", "2", "3"]] in
      replaceChars "hey" "yo" input `shouldBe` input

zippingLetters :: Spec
zippingLetters =
  describe "zipLetters" $ 
    it "Zipps letters correctly" $ 
      zipLetters " "
       [ [ ["a", "b", "c"], ["d", "e", "f"] ] 
       , [ ["g", "h", "i"], ["j", "k", "l"]
       ] 
       ]
      `shouldBe` 
       [ ["a", "b", "c", " ", "g", "h", "i"]
       , ["d", "e", "f", " ", "j", "k", "l"]
       ]