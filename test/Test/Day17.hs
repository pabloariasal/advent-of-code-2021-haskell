module Test.Day17 (tests) where

import Common
import Day17 (part1, part2)

input = "target area: x=20..30, y=-10..-5"

expected1 = "45"

expected2 = "112"

tests =
  createTests
    [ TestResult "Day 17 - Part 1" expected1 (part1 input),
      TestResult "Day 17 - Part 2" expected2 (part2 input)
    ]
