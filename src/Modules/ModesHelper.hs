module Modules.ModesHelper
  ( getMode
  )
  where

import           BotPrelude hiding (shift, note)
import qualified Data.Text  as T
import qualified Data.HashMap.Strict  as HM

getMode :: Text -> Text -> Either Text Text
getMode note mode = do
  eNote <- pure $ readNote note
  eIntervals <- pure $ HM.lookup mode $ HM.fromList
    [("ionian", ionian)
    ,("dorian", dorian)
    ,("phrygian", phrygian)
    ,("lydian", lydian)
    ,("mixolydian", mixolydian)
    ,("aeolian", aeolian)
    ,("minor", aeolian)
    ,("locrian", locrian)
    ]

  case (eNote, eIntervals) of
    (Just n, Just i) -> Right $ showT $ map showNote $ getMode' n i
    (_, _)           -> Left ("Incorrect input: '" <> note <> "' '" <> mode) 

data Note = C | Cs | Db | D | Ds | Eb | E | F | Fs | Gb | G | Gs | Ab | A | As | Bb | B
  deriving (Show, Read, Eq)

readNote :: Text -> Maybe Note
readNote s = readMaybe (T.unpack $ T.replace "#" "s" s) :: Maybe Note

showNote :: Note -> Text
showNote n = T.replace "s" "#" (showT n)

ionian :: [Int]
ionian = [2,2,1,2,2,2,1]
dorian :: [Int]
dorian = shift 1 ionian
phrygian :: [Int]
phrygian = shift 2 ionian
lydian :: [Int]
lydian = shift 3 ionian
mixolydian :: [Int]
mixolydian = shift 4 ionian
aeolian :: [Int]
aeolian = shift 5 ionian
locrian :: [Int]
locrian = shift 6 ionian

chromatic :: [Note]
chromatic = cycle [C, Db, D, Eb, E, F, Gb, G, Ab, A, Bb, B]

shift :: Int -> [Int] -> [Int]
shift 0 xs     = xs
shift i (x:xs) = shift (i - 1) (xs ++ [x])
shift _ _ = []

scaleOf :: Note -> [Note]
scaleOf key = dropWhile (\s -> s /= key) chromatic

getMode' :: Note -> [Int] -> [Note]
getMode' key ints = g ints (scaleOf key) []
  where
    g [] _ res = reverse res
    g (i:is) (n0:n1:ns) rs
      | i == 1 = g is (n1:ns) (n0:rs)
      | i == 2 = g is ns (n0:rs)
      | otherwise = []
    g _ _ _ = []

-- test :: IO ()
-- test = do
--   prnt $ getMode "A" "phrygian"
--   prnt $ getMode "as" "phrygian"
  -- prnt $ getMode' A ionian
  -- prnt $ getMode' A aeolian
  -- prnt $ getMode' Bb dorian
