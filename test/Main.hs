import Data.Foldable (null)
import Data.Map (Map)
import qualified Data.Map as M
import Data.Maybe (mapMaybe)
import System.Environment
import qualified Test.Day08 (tests)
import qualified Test.Day09 (tests)
import qualified Test.Day10 (tests)
import qualified Test.Day11 (tests)
import qualified Test.Day12 (tests)
import qualified Test.Day13 (tests)
import qualified Test.Day14 (tests)
import qualified Test.Day15 (tests)
import qualified Test.Day16 (tests)
import Test.HUnit

testcases :: Map String Test
testcases =
  M.fromList
    [ ("08", Test.Day08.tests),
      ("09", Test.Day09.tests),
      ("10", Test.Day10.tests),
      ("11", Test.Day11.tests),
      ("12", Test.Day12.tests),
      ("13", Test.Day13.tests),
      ("14", Test.Day14.tests),
      ("15", Test.Day15.tests),
      ("16", Test.Day16.tests)
    ]

gather :: [String] -> [Test]
gather ks
  | any (`M.notMember` testcases) ks = error "request test cases not implemented"
  | otherwise = mapMaybe (`M.lookup` testcases) ks

main :: IO ()
main = do
  args <- getArgs
  if null args
    then runTestTTAndExit (TestList $ gather (M.keys testcases))
    else runTestTTAndExit (TestList $ gather args)
