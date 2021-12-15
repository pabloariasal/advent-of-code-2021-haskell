module Test.Day13 (tests) where

import Common
import Day13 (part1, part2)

input = "6,10\n0,14\n9,10\n0,3\n10,4\n4,11\n6,0\n6,12\n4,1\n0,13\n10,12\n3,4\n3,0\n8,4\n1,10\n2,14\n8,10\n9,0\nfold along y = 7\n fold along x = 5"

expected1 = "17"

expected2 = "15"

tests =
  createTests
    [ TestResult "Day 13 - Part 1" expected1 (part1 input),
      TestResult "Day 13 - Part 2" expected2 (part2 input)
    ]
