{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}

module API.Types.Utils
  ( prettifyError
  , parseNested
  )
  where

import           API.Types.Error          (Error)
import           BotPrelude
import           Data.Aeson.Encode.Pretty (Config(..), Indent(..),
                                           NumberFormat(..), encodePretty',
                                           keyOrder)
import qualified Data.ByteString.Lazy     as LBS (toStrict)
import qualified Data.HashMap.Strict      as HM
import qualified Data.Text.Encoding       as E

prettifyError :: Error -> Text
prettifyError e = E.decodeUtf8 $ LBS.toStrict $ encodePretty' conf e
  where
    conf = Config
      { confIndent = Spaces 2
      , confCompare = keyOrder ["_error_msg", "_error_code", "_request_params", "key", "value"]
      , confNumFormat = Generic
      , confTrailingNewline = False
      }

parseNested :: Text -> (Value -> Parser a) -> Value -> Parser a
parseNested expected nestedParser (Object o) =
  case head $ HM.toList o of
    Just (actual, v) 
      | expected == actual -> nestedParser v
      | otherwise -> mzero
    _             -> mzero
parseNested _ _ _ = mzero
