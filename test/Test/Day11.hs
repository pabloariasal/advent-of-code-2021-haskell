module Test.Day11 (tests) where

import Common

import Day11 (part1, part2)

input ="5483143223\n2745854711\n5264556173\n6141336146\n6357385478\n4167524645\n2176841721\n6882881134\n4846848554\n5283751526\n"
expected1 = "1656"
expected2 = "195"

tests = createTests [TestResult "Day 11 - Part 1" expected1 (part1 input),
                     TestResult "Day 11 - Part 2" expected2 (part2 input)]
