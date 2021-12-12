module Common (createTests, TestResult(..)) where

import Test.HUnit

data TestResult = TestResult {name ::String, expected :: String, actual :: String} deriving (Show)

testCase :: TestResult -> Test
testCase (TestResult n e a) = TestCase (assertEqual n e a)

createTests :: [TestResult] -> Test
createTests = TestList . map testCase
