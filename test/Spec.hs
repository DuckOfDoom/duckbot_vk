import BotPrelude

import Test.Hspec

import qualified JSONTests
import qualified SlackEmotesTests

main :: IO ()
main = 
  hspec $ do 
    JSONTests.spec
    SlackEmotesTests.spec