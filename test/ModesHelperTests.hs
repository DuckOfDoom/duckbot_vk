module ModesHelperTests
( spec ) where

import BotPrelude
import Test.Hspec

import ParsingTestUtils (isParsedTo, fails)

import Modules.ModesHelper.Internal (parseNote, Note(..))

spec :: Spec
spec = describe "Module: ModesHelper" $ do
  inputParsing

inputParsing :: Spec 
inputParsing = 
    describe "parseNote" $ 
    let parse n = (parseNote, n) in do
      it "Parses notes correctly" $ do
        parse "C" `isParsedTo` C
        parse "f" `isParsedTo` F
        parse "A#" `isParsedTo` As
        parse "g#" `isParsedTo` Gs
        parse "Eb" `isParsedTo` Eb
        parse "db" `isParsedTo` Db
      it "Doesn't parse invalid input" $ do
        parse " " & fails
        parse "zb" & fails
        parse "123" & fails

    