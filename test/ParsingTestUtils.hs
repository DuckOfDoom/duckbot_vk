{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module ParsingTestUtils
  ( isParsedTo
  , fails
  , doesNotFail
  ) where

import BotPrelude
import Test.Hspec

import Data.Attoparsec.Text (Parser, parse, IResult(..))

import qualified Data.Text as T 
import qualified NeatInterpolation as F 

instance Eq a => Eq (IResult Text a) where
  (Done i1 r1) == (Done i2 r2) = i1 == i2 && r1 == r2
  (Fail{}) == (Fail{}) = True
  -- These two are added so that Parial results can be fully evaluated for testing
  -- since we need to pass empty string to parser as said in attoparsec docs
  x == (Partial f) = x == f T.empty
  (Partial f) == x = f T.empty == x

  _ == _ = False

-- Asserts that parser with a given input returns expected result
isParsedTo :: (Show a, Eq a) => (Parser a, Text) -> a -> Expectation
isParsedTo (parser, input) expected = parse parser input `shouldBe` Done "" expected

-- Asserts that parser with a given input fails
fails :: (Show a) => (Parser a, Text) -> Expectation
fails (parser, input) = checkResult $ parse parser input
  where 
    checkResult :: (Show a) => IResult Text a -> Expectation
    checkResult (Partial f) = checkResult $ f T.empty
    checkResult (Fail{}) = pure ()
    checkResult (Done i r) = expectationFailure $ 
      let result = show r in
      T.unpack [F.text|Parsing "$input" was expected to fail, but succeeded with result "$result" and remaining input "$i"|] 

-- Asserts that parsing succeeds
doesNotFail :: (Show a) => (Parser a, Text) -> Expectation
doesNotFail (parser, input) = checkResult $ parse parser input
  where 
    checkResult :: (Show a) => IResult Text a -> Expectation
    checkResult (Partial f) = checkResult $ f T.empty
    checkResult (Done{}) = pure ()
    checkResult (Fail i _ e) = expectationFailure $ 
      let 
        err = T.pack e
        in T.unpack [F.text|Parsing "$input" failed. Remaining input: "$i". Error: ${err}|] 