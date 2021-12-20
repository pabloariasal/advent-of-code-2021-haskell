module Test.Day15 (tests) where

import Common
import Day15 (part1, part2)

input = "1163751742\n1381373672\n2136511328\n3694931569\n7463417111\n1319128137\n1359912421\n3125421639\n1293138521\n2311944581\n"

expected1 = "40"

expected2 = "315"

tests =
  createTests
    [ TestResult "Day 15 - Part 1" expected1 (part1 input),
      TestResult "Day 15 - Part 2" expected2 (part2 input)
    ]
