module Modules.ModesHelper.Internal
  ( parseInput
  , parseNote
  , parseMode
  , mkResponse
  , modes
  , Note(..)
  ) where

import BotPrelude hiding (note, option, shift, take)

import Data.List  (lookup)
import Control.Monad (fail)

import Data.Attoparsec.Text (Parser, anyChar, char, peekChar, take, space)

import qualified Data.Char   as C
import qualified Data.Text   as T
import qualified Utils

modes :: [(Text, [Int], Text)]
modes =
  [("major", major, "Ionian - Major")
  ,("ionian", major, "Ionian - Major")
  ,("dorian", shift 1 major, "Dorian - Minor with a natural 6")
  ,("phrygian", shift 2 major, "Phrygian - Minor with a b2")
  ,("lydian", shift 3 major, "Lydian - Major with a #4")
  ,("mixolydian", shift 4 major, "Mixolydian - Major with a b7")
  ,("aeolian", shift 5 major, "Aeolian - Minor")
  ,("minor", shift 5 major, "Aeolian - Minor")
  ,("locrian", shift 6 major, "Locrian - Minor with b2 and b5")
  ]

data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
  deriving (Show, Read, Eq)

-- Tries to parse whole input (note with or without accidental and mode)
parseInput :: Parser (Note, ([Int], Text))
parseInput = do
  note <- parseNote 
  mode <- space >> parseMode 
  pure (note, mode)

-- Tries to read a note with or without accidental
parseNote :: Parser Note
parseNote = do
  note <- anyChar
  maybeAccidental <- peekChar
  accidental <- case maybeAccidental of
    Just ' ' -> pure Nothing
    Just _  -> Just <$> (char '#' <|> char 'b')
    Nothing -> pure Nothing
  case readNote (C.toUpper note) accidental of
    Nothing -> fail $ T.unpack ("Can't parse note" <> show note <> show accidental)
    Just n  -> pure n
  where
    readNote :: Char -> Maybe Char -> Maybe Note
    readNote note accidental =
      case accidental of
        Just a  -> readMaybe [note, replaceSharp a]
        Nothing -> readMaybe [note]
      where
        replaceSharp x = if x == '#' then 's' else x

-- Tries to read a mode name. Needs at least 3 characters
parseMode :: Parser ([Int], Text)
parseMode = do
  modeInput <- take 3
  let
    foundMode = find (\(name, _, _) -> T.toLower modeInput `T.isPrefixOf` name) modes
  case foundMode of
    Nothing -> fail $ T.unpack ("Can't parse mode '" <> modeInput <> "'")
    Just (_, intervals, comment) -> pure (intervals, comment)

-- Creates a response for user with a built mode and comment
mkResponse :: Note -> ([Int], Text) -> Text
mkResponse note (intervals, comment) =
  let mode = mkMode note intervals
      formattedMode = (Utils.joinText " " . map showNote) mode
    in
      Utils.joinText "\n" [showNote note <> " " <> comment, formattedMode]
  where
    showNote :: Note -> Text
    showNote n = T.replace "s" "#" (showT n)

mkMode :: Note -> [Int] -> [Note]
mkMode key intervals
  | isSharpKey key = map convertFlat mode
  | isFlatKey key = map convertSharp mode
  | otherwise = mode
  where
    mode = buildMode intervals getScaleForKey []
    -- Recursively builds a mode from a scale by shifting it by intervals from the list
    buildMode :: [Int] -> [Note] -> [Note] -> [Note]
    buildMode [] _ result = reverse result
    buildMode (i:is) (n0:n1:ns) rs
      | i == 1 = buildMode is (n1:ns) (n0:rs)
      | i == 2 = buildMode is ns (n0:rs)
      | otherwise = []
    buildMode _ _ _ = []

    getScaleForKey :: [Note]
    getScaleForKey = dropWhile (\s -> s /= convertSharp key) chromatic
      where
        chromatic = cycle [C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B]

-- by the circle of fifths and common sense
isSharpKey :: Note -> Bool
isSharpKey k = k `elem` [C, Cs, D, Ds, G, Gs, A, As, E, B, Fs]

isFlatKey :: Note -> Bool
isFlatKey k = k `elem` [Db, Eb, F, Gb, Ab, Bb]

sharpsToFlats :: [(Note, Note)]
sharpsToFlats = [(Cs, Db), (Ds, Eb), (Fs, Gb), (Gs, Ab), (As, Bb)]

convertSharp :: Note -> Note
convertSharp n = (fromMaybe n . lookup n) sharpsToFlats

convertFlat :: Note -> Note
convertFlat n = (fromMaybe n . lookup n . map (\(s,f) -> (f,s))) sharpsToFlats

major :: [Int]
major = [2,2,1,2,2,2,1]

shift :: Int -> [Int] -> [Int]
shift 0 xs     = xs
shift i (x:xs) = shift (i - 1) (xs ++ [x])
shift _ _      = []
