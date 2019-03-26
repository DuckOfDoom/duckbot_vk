module ModesHelperTests
( spec ) where

import BotPrelude
import Test.Hspec

import ParsingTestUtils (isParsedTo, fails, doesNotFail)
import Modules.ModesHelper.Internal (parseNote, parseMode, modes, Note(..))

import qualified Data.Text as T

spec :: Spec
spec = describe "Module: ModesHelper" $ do
  inputParsing

inputParsing :: Spec 
inputParsing = 
  describe "parseNote" $ do
  let parse t = (parseNote, t) in do
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

  describe "parseMode" $ 
    let parse t = (parseMode, t) in do
    it "Parses modes correctly" $ 
      mapM_ (\(m, _, _) -> parse (T.take 3 m) & doesNotFail) modes
      -- parse "dor" `isParsedTo` 
    