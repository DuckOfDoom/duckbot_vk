module Modules.ModesHelper
  where

import           BotPrelude hiding (note, shift)
import           Data.List  (lookup)
import qualified Data.Text  as T
import qualified Utils

getMode :: (Text, Text) -> Either Text Text
getMode (note, mode) =
  case (readNote, findMode) of
    (Just note', Just (_, intervals, comment)) ->
      let
        rMode = getMode' note' intervals
        formattedMode = (Utils.joinText " " . map showNote) rMode
        in
          Right $ Utils.joinText " " [formattedMode, comment]

    (_, Nothing)           -> Left $ "Can't read mode: " <> mode
    (Nothing, Just _)           -> Left $ "Can't read note: " <> note
    where
      showNote :: Note -> Text
      showNote n = T.replace "s" "#" (showT n)

      readNote :: Maybe Note
      -- TODO: Fix reading for B!
      readNote
       | T.null note = Nothing
       | T.length note == 1 = (readMaybe . T.unpack . T.toUpper) note :: Maybe Note
       | otherwise = (readMaybe . replaceSymbol . T.unpack . T.toUpper) note :: Maybe Note
        where
          replaceSymbol (x:y)
            | y == "#" = x:"s"
            | y == "B" = x:"b"
            | otherwise = ""
          replaceSymbol _ = ""

      findMode :: Maybe (Text, [Int], Text)
      findMode = find (\(name, _, _) -> T.toLower mode `T.isPrefixOf` name)
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

getMode' :: Note -> [Int] -> [Note]
getMode' key ints
  | isSharpKey key = map convertFlat mode
  | isFlatKey key = map convertSharp mode
  | otherwise = mode
  where
    mode = buildMode ints getScaleForKey []
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
