module Test.Day09 (tests) where

import Test.HUnit

import Day09 (part1, part2)

input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
expected1 = "15"
expected2 = "1134"

test1 :: Test
test1 = TestCase (assertEqual "Example" expected1 (part1 input))

test2 :: Test
test2 = TestCase (assertEqual "Example" expected2 (part2 input))

tests :: Test
tests = TestList [TestLabel "Part 1" test1, TestLabel "Part 2" test2]
