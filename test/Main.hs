import Data.Foldable (null)
import qualified Data.Map as Map
import Data.Maybe (mapMaybe)
import System.Environment
import qualified Test.Day09 (tests)
import qualified Test.Day10 (tests)
import Test.HUnit

testcases :: Map.Map String Test
testcases = Map.fromList [("09", Test.Day09.tests), ("10", Test.Day10.tests)]

gather :: [String] -> [Test]
gather = mapMaybe (`Map.lookup` testcases)

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runTestTTAndExit (TestList $ gather (Map.keys testcases))
    else runTestTTAndExit (TestList $ gather args)
