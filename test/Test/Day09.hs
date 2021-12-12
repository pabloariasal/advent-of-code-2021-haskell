module Test.Day09 (tests) where

import Common

import Day09 (part1, part2)

input = "2199943210\n3987894921\n9856789892\n8767896789\n9899965678"
expected1 = "15"
expected2 = "1134"

tests = createTests [TestResult "Day 09 - Part 1" expected1 (part1 input),
                     TestResult "Day 09 - Part 2" expected2 (part2 input)]
