{-# LANGUAGE FlexibleInstances #-}
-- Guess i really need that instance
{-# OPTIONS_GHC -fno-warn-orphans #-}

module SlackEmotesTests
( spec ) where

import BotPrelude
import Test.Hspec

import Modules.SlackEmotes.Internal (replaceChars, zipLetters, inputParser)
import Data.Attoparsec.Text (parse, parseOnly, IResult(..))
import qualified Data.Text as T

instance Eq a => Eq (IResult Text a) where
  (Done i1 r1) == (Done i2 r2) = i1 == i2 && r1 == r2
  (Fail i1 c1 e1) == (Fail i2 c2 e2) = i1 == i2 && c1 == c2 && e1 == e2

  -- These two are added so that Parial results can be fully evaluated for testing
  x == (Partial f) = x == f ""
  (Partial f) == x = f "" == x

  _ == _ = False

spec :: Spec
spec = describe "Module: SlackEmotes" $ do
  inputParsing
  replacingCharacters
  zippingLetters

inputParsing :: Spec
inputParsing = 
  describe "inputParser" $ do
    it "Parses input correctly" $ do
      let input = "The quick brown fox jumps over the lazy dog"
      parse inputParser (input <> " :monkas: :hey_yo:") `shouldBe` Done "" (T.toLower input, ":monkas:", ":hey_yo:")
      -- should treat any case as lower (we have only one alphabet anyways)
      parse inputParser (T.toUpper input <> " :monkas: :hey_yo:") `shouldBe` Done "" (T.toLower input, ":monkas:", ":hey_yo:")
      parse inputParser "a :monkas:" `shouldBe` Done "" ("a", ":monkas:", ":white_small_square:")
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