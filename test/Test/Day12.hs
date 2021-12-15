module Test.Day12 (tests) where

import Common
import Day12 (part1, part2)

input = "start-A\nstart-b\nA-c\nA-b\nb-d\nA-end\nb-end"

expected1 = "10"

expected2 = "36"

tests =
  createTests
    [ TestResult "Day 12 - Part 1" expected1 (part1 input),
      TestResult "Day 12 - Part 2" expected2 (part2 input)
    ]
