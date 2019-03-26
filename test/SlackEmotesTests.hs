{-# LANGUAGE FlexibleInstances #-}
-- Guess i really need that instance

module SlackEmotesTests
( spec ) where

import BotPrelude
import Test.Hspec
import ParsingTestUtils

import Modules.SlackEmotes.Internal (replaceChars, zipLetters, inputParser)
import qualified Data.Text as T

spec :: Spec
spec = describe "Module: SlackEmotes" $ do
  inputParsing
  replacingCharacters
  zippingLetters

inputParsing :: Spec
inputParsing = 
  describe "inputParser" $ 
    let parse i = (inputParser, i) in do
      it "Parses input correctly" $ 
        let input = "The quick brown fox jumps over the lazy dog" in do
          parse (input <> " :monkas: :hey_yo:") `isParsedTo` (T.toLower input, ":monkas:", ":hey_yo:")
        -- should treat any case as lower (we have only one alphabet anyways)
          parse (T.toUpper input <> " :monkas: :hey_yo:") `isParsedTo` (T.toLower input, ":monkas:", ":hey_yo:")
          parse "a :monkas:" `isParsedTo` ("a", ":monkas:", ":white_small_square:")
      it "Does not parse input with special symbols" $ 
        fails $ parse "#123 :text: :text2:" 
      it "Does not parse incorrect input" $ do
        parse "derp" & fails
        parse "text text2" & fails
        parse "text text1 text2" & fails
        parse "" & fails

replacingCharacters :: Spec
replacingCharacters = 
  describe "replaceChars" $ do
    it "Replaces characters correctly" $ 
      replaceChars [[" ", "#", "#"], ["#", " ", " "]] "hey" "yo" `shouldBe` [["yo", "hey", "hey"], ["hey", "yo", "yo"]]
    it "Leaves incorrect chars as is" $ 
      let input = [["1", "2", "3"], ["1", "2", "3"]] in
      replaceChars input "hey" "yo" `shouldBe` input

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