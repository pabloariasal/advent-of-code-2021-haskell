module Test.Day16 (tests) where

import Common
import Day16 (part1, part2)

input1 = "A0016C880162017C3686B18A3D4780"

expected1 = "31"

input2 = "880086C3E88112"

expected2 = "7"

input3 = "9C0141080250320F1802104A08"

expected3 = "1"

input4 = "C200B40A82"

expected4 = "3"

tests =
  createTests
    [ TestResult "Day 16 - Part 1" expected1 (part1 input1),
      TestResult "Day 16 - Part 2-1" expected2 (part2 input2),
      TestResult "Day 16 - Part 2-2" expected3 (part2 input3),
      TestResult "Day 16 - Part 2-3" expected4 (part2 input4)
    ]
