module ModesHelperTests
( spec ) where

import BotPrelude
import Test.Hspec

import Modules.ModesHelper.Internal (parseInput, parseNote, parseMode, modes, Note(..))
import ParsingTestUtils (isParsedTo, fails)

import qualified Data.Text as T

spec :: Spec
spec = describe "Module: ModesHelper" $ 
  inputParsing

inputParsing :: Spec 
inputParsing = do
  describe "parseNote" $ 
    let parse t = (parseNote, t) in do
    it "Parses notes correctly" $ do
      parse "C" `isParsedTo` C
      parse "f" `isParsedTo` F
      parse "A#" `isParsedTo` As
      parse "g#" `isParsedTo` Gs
      parse "Eb" `isParsedTo` Eb
      parse "db" `isParsedTo` Db
    it "Fails parsing invalid input" $ do
      parse " " & fails
      parse "zb" & fails
      parse "123" & fails

  describe "parseMode" $ 
    let parse t = (parseMode, t) in do
    it "Parses modes correctly" $ 
      mapM_ (\(m, ints, comment) -> parse (T.take 3 m) `isParsedTo` (ints, comment)) modes
    it "Fails parsing invalid input" $ do
      parse "" & fails
      parse " " & fails
      parse "derp" & fails

  describe "parseInput" $ 
    let parse t = (parseInput, t) in do
    it "Parses whole input without accidentals" $ do
      parse "c maj" `isParsedTo` (C, ([2,2,1,2,2,2,1], "Ionian - Major"))
      parse "F mix" `isParsedTo` (F, ([2,2,1,2,2,1,2], "Mixolydian - Major with a b7"))
    it "Parses whole input with accidentals" $ do
      parse "Bb min" `isParsedTo` (Bb, ([2,1,2,2,1,2,2], "Aeolian - Minor"))
      parse "G# phr" `isParsedTo` (Gs, ([1,2,2,2,1,2,2], "Phrygian - Minor with a b2"))
-- it "Fails parsing invalid input"