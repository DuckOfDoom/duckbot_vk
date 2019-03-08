module Modules.SlackEmotes.Letters where

import BotPrelude

import Data.HashMap.Strict (HashMap, keys, fromList, lookup) 

getLetter :: Char -> [[Text]]
getLetter c = fromMaybe unknownLetter (lookup c letters)

availableLetters :: [Char] 
availableLetters = keys letters

unknownLetter :: [[Text]]
unknownLetter = 
  [["#","#","#","#","#"]
   ,["#","#"," ","#","#"]
   ,["#"," ","#"," ","#"]
   ,["#","#"," ","#","#"]
   ,["#","#","#","#","#"]]

letters :: HashMap Char [[Text]]
letters = fromList
  [('a'
  ,[[" "," ","#"," "," "]
   ,[" ","#"," ","#"," "]
   ,["#"," "," "," ","#"]
   ,["#","#","#","#","#"]
   ,["#"," "," "," ","#"]]
  )
  ,('b'
  ,[[" "," ","#"," "," "]
   ,[" ","#"," ","#"," "]
   ,["#"," "," "," ","#"]
   ,["#","#","#","#","#"]
   ,["#"," "," "," ","#"]]
  )
  ]