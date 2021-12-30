module Test.Day21 (tests) where

import Common
import Day21 (part1, part2)

input = "Player 1 starting position: 4\nPlayer 2 starting position: 8"

expected1 = "739785"

expected2 = "444356092776315"

tests =
  createTests
    [ TestResult "Day 21 - Part 1" expected1 (part1 input),
      TestResult "Day 21 - Part 2" expected2 (part2 input)
    ]
