import BotPrelude

import Test.Hspec

import qualified JSONTests
import qualified SlackEmotesTests
import qualified ModesHelperTests

main :: IO ()
main = 
  hspec $ do 
    JSONTests.spec
    SlackEmotesTests.spec
    ModesHelperTests.spec