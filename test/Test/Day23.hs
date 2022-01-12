module Test.Day23 (tests) where

import Common
import Day23 (part1, part2)

input =
  "#############\n\
  \#...........#\n\
  \###B#C#B#D###\n\
  \#A#D#C#A#\n\
  \#########"

expected1 = "12521"

expected2 = "15"

tests =
  createTests
    [ TestResult "Day 23 - Part 1" expected1 (part1 input),
      TestResult "Day 23 - Part 2" expected2 (part2 input)
    ]
